void mult(char * a, char * b, char * c, int sz, int y) {
  int i,j,k,ij,ik,kj;
  for(i = 0; i < sz; i++){
    for(j = 0; j < sz; j++){
      ij = i * sz + j;
      c[ij] = 0;
      for(k = 0; k < sz; k++){
        ik = i * sz + k;
        kj = k * sz + j;
        c[ij] = c[ij] || (a[ik] && b[kj]);
      }
    }
  }
}
