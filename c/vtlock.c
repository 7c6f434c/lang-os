#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <linux/vt.h>

int main (int argc, char** argv) {
        int consfd = -1;
        int op;

        consfd = open("/dev/console", O_RDWR);
        if (consfd == -1){
                fprintf(stderr, "Oops, cannot open /dev/console");
                return 1;
        }

        if(argc<=1){
                op = VT_UNLOCKSWITCH;
        }else{
                switch(argv[1][0]){
                        case '\0': op = VT_UNLOCKSWITCH; break;
                        case 'u': op = VT_UNLOCKSWITCH; break;
                        case 'l': op = VT_LOCKSWITCH; break;
                        case '-': op = VT_UNLOCKSWITCH; break;
                        case '+': op = VT_LOCKSWITCH; break;
                        case '0': op = VT_UNLOCKSWITCH; break;
                        case '1': op = VT_LOCKSWITCH; break;
                        case 'n': op = VT_UNLOCKSWITCH; break;
                        case 'y': op = VT_LOCKSWITCH; break;
                        case 'o': 
                                  if (argv[1][1]=='n'){
                                          op = VT_LOCKSWITCH;
                                  }else{
                                          op = VT_UNLOCKSWITCH;
                                  }
                                  break;
                        default: op = VT_UNLOCKSWITCH; break;
                }
        }

        if(ioctl(consfd, op)==-1){
                fprintf(stderr, "Oops, ioctl failed\n");
                return 2;
        }
        return 0;
}

