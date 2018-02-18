#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <assert.h>

int main (int argc, char** argv){
        assert(argc>=2);
        int fd = open(argv[1], O_RDWR | O_CREAT, 0600);
        assert(fd != -1);

        flock(fd, LOCK_EX);
        assert(write(1,"OK\n",3)==3);
        char in;
        assert(read(0,&in,1)==1);
        flock(fd, LOCK_UN);
        return 0;
} 
