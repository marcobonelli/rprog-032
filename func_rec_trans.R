P = matrix(0:0, 6, 6)
	P[1, ] = c(0.3, 0.5, 0, 0, 0, 0.2)
	P[2, ] = c(0, 0.5, 0, 0.5, 0, 0)
	P[3, ] = c(0, 0, 1, 0, 0, 0)
	P[4, ] = c(0, 0.3, 0, 0, 0, 0.7)
	P[5, ] = c(0.1, 0, 0.1, 0, 0.8, 0)
	P[6, ] = c(0, 1, 0, 0, 0, 0)

calc_rec_trans = function(P){
	m = ncol(P)								
	stateNames = rownames(P)					
    T = zeros(m)								
    i = 1									
    while(i <= m){							
		a = i						
        b = zeros(1,m)
        b[1, i] = 1								
        old = 1								
        new = 0								
		
        while(old != new){					
			old = sum(find(b > 0))				
            n = size(a)[2]						
            matr = matrix(as.numeric(P[a, ]), ncol = m, nrow = n)
            c = colSums(matr)					
            d = find(c)						
            n = size(d)[2]						
            b[1, d] = ones(1, n)					
            new = sum(find(b > 0))				
            a = d								
        }

        T[i, ] = b								
        i = i + 1							
	}
    F = t(T)									
    C = (T > 0) & (F > 0)						
    v = (apply(t(C) == t(T), 2, sum) == m)                      
    j = return(v)
}

j = calc_rec_trans(P)						
b = numeric() 									
a = numeric() 								
n = ncol(P)									

for(i in 1 : n){
	if (j[i] == "TRUE"){b[i] = i} 				
	else{a[i] = i}								
}

b[is.na(b) == T] = 0								
recorrentes = b[b > 0]								
print(recorrentes)

a[is.na(a) == T] = 0								
transientes = a[a > 0]								
print(transientes)
