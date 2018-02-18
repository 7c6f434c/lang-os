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
        int vtn = 0;
        int sleeptime = 0;
        FILE* consio;
        char vtname[100];

        if(argc==1){
                printf("I want a vt number\n");
                return 1;
        }
        vtn = atoi(argv[1]);
        if(vtn==0){
                printf("I want a numeric vt number\n");
                return 1;
        }

        snprintf(vtname, 20, "/dev/tty%d", vtn);

        printf("VT name is %s\n", vtname);
        consio = fopen(vtname, "r+");

        if(consio == NULL){
                printf("Oops, cannot open the vt\n");
                return 2;
        }

        consfd = open("/dev/console", O_RDWR);
        if (consfd == -1){
                printf("Oops, cannot open /dev/console");
                return 2;
        }

        printf("Starting\n");
        /* just in case */
        ioctl(consfd, VT_UNLOCKSWITCH);
        if(ioctl(consfd, VT_ACTIVATE, vtn)==-1){
                printf("Couldn't switch console\n");
                return 3;
        }
        fprintf(consio,"Starting\n");
        if(ioctl(consfd, VT_LOCKSWITCH)==-1){
                printf("Couldn't lock console\n");
                fprintf(consio, "Couldn't lock console\n");
                return 3;
        }
        printf("Mwahaha?\n");
        fprintf(consio, "Mwahaha?\n");

        if(argc>2){
                sleeptime = atoi(argv[2]);
        }
        if(sleeptime==0){
                sleeptime = 5;
        };
        sleep(sleeptime);

        printf("Finishing\n");
        fprintf(consio,"Finishing\n");
        if(ioctl(consfd, VT_UNLOCKSWITCH)==-1){
                printf("Couldn't unlock console, strange\n");
                fprintf(consio, "Couldn't unlock console, strange\n");
                return 4;
        }
        printf("Bye\n");
        fprintf(consio,"Bye\n");
        return 0;
}
