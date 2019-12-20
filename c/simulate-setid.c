#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <grp.h>

int main ( int argc, char**argv){
        assert(argc>3);

        int uid=atoi(argv[1]);
        int gid=atoi(argv[2]);

        assert(uid);
        if(gid==0) gid = 65534;

	if(gid>0){
        	assert(setregid(gid,0)==0);

		assert(setgroups(0,NULL)==0);
	}

        assert(setreuid(uid,0)==0);

        return execvp(argv[3],argv+3);
}
