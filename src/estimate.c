#include <stdio.h>
#include <stdlib.h>
#include <string.h>


double ** transpose(double ** m,int rows,int columns);
double ** multiply(double ** m1,double ** m2,int rows1,int rows2,int columns);
double * vmultiply(double ** m,double * v,int rows,int columns);
double ** inverse(double ** m,int rows);

double ** inverse(double ** m,int size) {

    double ** ans = (double **) malloc(size * sizeof(double *));

    int i,j,k;
    for (i = 0; i < size; i++) {
        ans[i] = (double *) malloc(size * sizeof(double));
        for (j = 0; j < size; j++) {
            if (i == j) {
                ans[i][j] = 1;
            } else {
                ans[i][j] = 0;
            }
        }
    }

    for (int i = 0; i < size; i++) {
        double rec; 
        if (m[i][i] != 1) {
            rec = 1/m[i][i];
            for (j = 0; j < size; j++) {
                m[i][j] *= rec;
                ans[i][j] *= rec; 
                if (m[i][j] == -0) m[i][j] = 0;
                if (ans[i][j] == -0) ans[i][j] = 0;
            }
        }


        for (k = i+1; k < size; k++) {
            double f; 
            if (m[k][i] != 0) {
                f = m[k][i] * -1;


                int l;
                for (l = 0; l < size; l++) {
                    m[k][l] += (f*m[i][l]);
                    ans[k][l] += (f*ans[i][l]);

                    if (m[k][l] == -0) m[k][l] = 0;
                    if (ans[k][l] == -0) ans[k][l] = 0;
                }
            }
        }
    }


   for (i = size-1; i >= 0; i--) {
       double rec; 
       if (m[i][i] != 1) {
           rec = 1/m[i][i];

           for (j = 0; j < size; j++) {
               m[i][i] *= rec;
               ans[i][i] *= rec;

               if (m[i][j] == -0) m[i][j] = 0;
               if (ans[i][j] == -0) ans[i][j] = 0;
           }
       }


       for (k = i-1; k >= 0; k--) {
            double f;
            if (m[k][i] != 0) {
                f = m[k][i] * -1;


                int l;
                for (l = 0; l < size; l++) {
                    m[k][l] += (f*m[i][l]);
                    ans[k][l] += (f*ans[i][l]);


                    if (m[k][l] == -0) m[k][l] = 0;
                    if (ans[k][l] == -0) ans[k][l] = 0;
                }
            }
       }
   }
  
   return ans;
   }
   
   double ** multiply(double ** m1,double ** m2,int rows1,int rows2,int columns) {
    double ** t = (double **) malloc(rows1 * sizeof(double *));

    int i,j,k;
    for (i = 0; i < rows1; i++) {
        t[i] = (double *) malloc(columns * sizeof(double));

        for (j = 0; j < columns; j++) {
            t[i][j] = 0;
        }
    }

    for (i = 0; i < rows1; i++) {
        for (j = 0; j < columns; j++) {
            for (k = 0; k < rows2; k++) {
                t[i][j] += m1[i][k] * m2[k][j];
            }
        }
    }
    return t;
}

double ** transpose(double ** m,int rows,int columns) {
    double ** t = (double **) malloc(columns * sizeof(double *));

    int i,j;
    for (i = 0; i < columns; i++) {
        t[i] = (double *) malloc(rows * sizeof(double));
    }

    for (i = 0; i < columns; i++) {
        for (j = 0; j < rows; j++) {
            t[i][j] = m[j][i];
        }
    }

    return t;
}

double * vmultiply(double ** m,double * v,int rows,int columns) {
    double * t = (double *) malloc(rows * sizeof(double));
    int i,j;

    for (i = 0; i < rows; i++) {
    	t[i] = 0;
        for (j = 0; j < columns; j++) {
            t[i] += m[i][j] * v[j];
        }
    }
    return t;
}


int main(int argc,char ** argv) {
    FILE * f1 = NULL; 
    FILE * f2 = NULL;
    FILE * t = NULL;
    FILE * d = NULL;
    char * filename1 = NULL;
    char * filename2 = NULL;
    char label[20] = {}; 
    int columns,rows, dataColumns, dataRows;
    double ** loader; 
    double ** X; 
    double ** data; 
    double * Y; 
    double * W; 
    double ** tr; 
    double ** in; 
    double ** r1; 
    double ** r2;
    double * r3;

    if (argc != 3) {
        printf("error\n");
        exit(-1);
    }

    filename1 = argv[1];
    filename2 = argv[2];


    f1 = fopen(filename1,"r");
    f2 = fopen(filename2,"r");


    if (f1 == NULL || f2 == NULL) {
        printf("error\n");
        exit(-1);
    }

    fscanf(f1,"%s\n",label);


    if ((strncmp(label,"train",5) == 0)) {
        t = f1;
        d = f2;
    } else {
        printf("Pass train file before data file.\n");
    }


    fscanf(t,"%d\n",&columns);
    fscanf(t,"%d\n",&rows);
    columns++; 


    X = (double **) malloc(rows * sizeof(double *));
    loader = (double **) malloc(rows * sizeof(double *));
    Y = (double *) malloc(rows * sizeof(double *));

    int i;
    for (i = 0; i < rows; i++) {
        X[i] = (double *) malloc(columns * sizeof(double));
        loader[i] = (double *) malloc(columns * sizeof(double));
        X[i][0] = 1; 
    }


    int j;
    for (i = 0; i < rows; i++) {
        for (j = 0; j < columns; j++) {
            fscanf(t,"%lf ",&loader[i][j]);
        }
        fscanf(t,"\n");
    }


    for (i = 0; i < rows; i++) {
        for (j = 1; j < columns; j++) {
            X[i][j] = loader[i][j-1];
        }
    }


    for (i = 0; i < rows; i++) {
        Y[i] = loader[i][columns-1];
    }

    tr = transpose(X,rows,columns);
    r1 = multiply(tr,X,columns,rows,columns);
    in = inverse(r1,columns);
    r2 = multiply(in,tr,columns,columns,rows);
    W = vmultiply(r2,Y,columns,rows);

    fscanf(d,"%s\n",label);


    if (strncmp(label,"data",4) != 0) {
        printf("error\n");
        exit(0);
    }

    fscanf(d,"%d\n",&dataColumns);
    fscanf(d,"%d\n",&dataRows);
    dataColumns++; 


    data = (double **) malloc(dataRows * sizeof(double *));


    for (i = 0; i < dataRows; i++) {
        data[i] = (double *) malloc(dataColumns * sizeof(double));
        data[i][0] = 1; 
    }

    for (i = 0; i < dataRows; i++) {
        for (j = 1; j < dataColumns; j++) {
            fscanf(d,"%lf ",&data[i][j]);
        }
        fscanf(d,"\n");
    }
    
    r3 = vmultiply(data,W,dataRows,dataColumns);

    for (i = 0; i < dataRows; i++) {
        printf("%.0f\n",r3[i]);
    }
    
    
     //free all matrices' memory
    for (i = 0; i < rows; i++) {
        free(loader[i]);
        free(X[i]);
        }
        free(loader);
        free(X);
        free(Y);
        
    for (i = 0; i < dataRows; i++) {
    	free(data[i]);
        }
        free(data);
        
    for (i = 0; i < columns; i++) {
    	free(tr[i]);
    	free(in[i]);
    	free(r1[i]);
    	free(r2[i]);
    	}
    free(W);  
    free(tr);
    free(in);
    free(r1);
    free(r2);
        
    free(r3);
    
    return 0;
}






 




