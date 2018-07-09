#include <stdio.h>
#include <stdlib.h>

int Fib_iter(int n) {
	int k, i = 1, F = 0;
	
	for (k = 1; k <= n; k++) {
		F = F + i;
		i = F - i;
	}
	return F;
}

int main(){
	// {0,1,1,2,3,5,8,13,21} 
	int x = Fib_iter(4);
	printf("%d\n",x );


	return 0;
}