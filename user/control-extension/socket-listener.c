#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <poll.h>
#include <assert.h>
#include <cjson/cJSON.h>

#define BUFFER_SIZE (64*1024*1024+1024+1)
#define OP_SIZE (BUFFER_SIZE-256)
#define INPUT_SIZE (1024*1024 - 1024)

int main(int argc, char**argv) {
    int server_fd, client_fd;
    struct sockaddr_un addr;
    socklen_t addr_size;
    
    if(argc<2){
            perror("no socket path");
            exit(EXIT_FAILURE);
    }

    if ((server_fd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        perror("socket error");
        exit(EXIT_FAILURE);
    }

    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, argv[1], sizeof(addr.sun_path) - 1);

    unlink(argv[1]);

    if (bind(server_fd, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
        perror("bind error");
        exit(EXIT_FAILURE);
    }

    if (listen(server_fd, 5) == -1) {
        perror("listen error");
        exit(EXIT_FAILURE);
    }
    
    chmod(argv[1],0777);

    addr_size = sizeof(struct sockaddr_un);
    while ((client_fd = accept(server_fd, (struct sockaddr*)&addr, &addr_size)) != -1) {
            pid_t pid=fork();
            if(pid){
                    waitpid(pid, NULL, 0);
                    fprintf(stderr, "Child %d finished\n", pid);
                    addr_size = sizeof(struct sockaddr_un);
                    continue;
            }
            fprintf(stderr, "Child forked\n");
            char * buffer = malloc(BUFFER_SIZE);
            fprintf(stderr, "allocated: %p\n", buffer);
            assert(buffer);

            FILE * client_r = fdopen(dup(client_fd), "r");

            int32_t len = 0;

            len=1;
            while(len){
                    // Read data from the connected client, send it to the extension
                    if(fgets(buffer, INPUT_SIZE, client_r)!=NULL){
                            len = strlen(buffer);
                            len--; // Newline at the end
                            fprintf(stderr, "Read with len %d\n", len);
                            if(len>0){
                                    cJSON * parsed = cJSON_ParseWithLength(buffer,len);
                                    if(parsed){
                                            cJSON_Delete(parsed);
                                            if(write(1, &len, sizeof(len))!=sizeof(len) ||
                                                            write(1, buffer, len) != len){
                                                    len = 0; 
                                                    goto drop_client;
                                            }
                                    }else{
                                            perror("invalid JSON");
                                            len = 0; 
                                            goto drop_client;
                                    }
                            }else{
                                    perror("refusing empty write");
                                    len = 0; 
                                    goto drop_client;
                            }
                    }else{
                            perror("empty read from input");
                            len = 0;
                            goto drop_client;
                    }

                    // Forward extension responses to the socket client
                    if ((read(0, &len, 4) == 4) && (len<OP_SIZE)) {
                            fprintf(stderr, "Reply with len %d\n", len);
                            int totlen = 0;
                            int lastlen = 0;
                            while(((totlen+=lastlen)<len)
                                            && 
                                            ((lastlen=read(0
                                                           ,buffer+totlen
                                                           ,len-totlen))>0))
                            {
                            }
                            if(totlen == len){
                                    buffer[len]='\n';
                                    buffer[len+1]='\0';
                                    len+=1;
                                    int totlen = 0;
                                    int lastlen = 0;
                                    while(((totlen+=lastlen)<len)
                                                    && 
                                                    ((lastlen=write(client_fd
                                                                   ,buffer+totlen
                                                                   ,len-totlen))>0))
                                    {
                                    }
                                    if(totlen<len){
                                            perror("response body not sent");
                                            fprintf(stderr, "Sent only %d bytes of %d\n", totlen, len);
                                            len = 0;
                                            goto drop_client;
                                    }
                            }else{
                                    perror("response body not read");
                                    len = 0;
                                    goto drop_client;
                            }
                    } else {
                            perror("no response header");
                            len = 0;
                            goto drop_client;
                    }
            }

            drop_client:

            int ret=1;
            while(ret>0){
                    struct pollfd fds;
                    fds.fd = 0; // stdin
                    fds.events = POLLIN; // check for input
                    ret = poll(&fds, 1, 0); // timeout 0 means return immediately
                    if (ret == -1) {
                            perror("poll");
                            exit(EXIT_FAILURE);
                    }
                    if(ret>0){
                            if(read(0,buffer,OP_SIZE)<=0){
                                    ret=0;
                            }
                    }
            }


            fclose(client_r);
            close(client_fd);
            exit(EXIT_SUCCESS);
    }
    close(server_fd);

    return 0;
}
