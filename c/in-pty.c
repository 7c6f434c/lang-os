#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <pty.h>
#include <stdlib.h>

pid_t readpid=0,writepid=0,shellpid=0;

void sigforward(int sig){
	if(shellpid){
		kill(shellpid,sig);
	}
}

int main(int argc, char ** argv, char ** envp)
{
	int fd;
	char readbuf[1025], writebuf[1025];
	signed long int readc, forwardc, writec;
	int exitstatus;

	if(!(shellpid=forkpty(&fd, NULL, NULL, NULL))){
		execvp(argv[1], argv+1);
	}

	if (!(readpid=fork())){
		readc = 1;
		forwardc = 1;
		while(readc>0 && forwardc>0) {
			readc=read(0, readbuf, 1024);
			if(readc>0) {
				forwardc = write(fd, readbuf, readc);
			} else {
				readbuf[0] = '\4';
				forwardc = write(fd, readbuf, 1);
			}
		}
		close(0);
		close(fd);
		while(1) { sleep(1024);}
	}

	if (!(writepid=fork())){
                writec = 1;
                forwardc = 1;
		while(writec>=0 && forwardc>0) {
			writec=read(fd, writebuf, 1024);
			if(writec>0) {
				do {
					forwardc = write(1, writebuf, writec);
				} while(forwardc==-1 && 
						(errno == EAGAIN ||
						 errno == EWOULDBLOCK));
			}else{
				if(errno == EAGAIN || errno == EWOULDBLOCK){
					writec = 0;
				}
				forwardc = 1;
			}
			
		}
                close(fd);
                close(1);
		exit(0);
	}

	signal(SIGTERM, &sigforward);
	signal(SIGINT, &sigforward);
	signal(SIGQUIT, &sigforward);
	signal(SIGHUP, &sigforward);
	signal(SIGPIPE, &sigforward);

	waitpid(shellpid, &exitstatus, 0);

	kill(readpid, SIGTERM);
	waitpid(readpid, NULL, 0);
	waitpid(writepid, NULL, 0);

	return WEXITSTATUS(exitstatus);
}
