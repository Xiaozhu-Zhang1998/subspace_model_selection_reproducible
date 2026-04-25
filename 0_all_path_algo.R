compute_trace = function(S, j, bags) {
  if (length(S) !=0 ) {
    U = svd(X[,S])$u
    vj =  X[,j] - U %*% t(U) %*% X[,j] 
  } else {
    vj = X[,j]
  }
  return( (t(vj) %*% bags$Sinfo %*% vj) / norm(vj, "2")^2 )
}

compute_pi = function(S, X, bags) {
  if (length(S) == 1) {
    vj =  X[,S]
    return( (t(vj) %*% bags$Sinfo %*% vj) / norm(vj, "2")^2 )
  }
  
  Z = X[,S]
  base_lst = bags$base_lst
  Uc = svd(Z)$u
  Mat = matrix(0, nrow = nrow(Z), ncol = nrow(Z))
  for(i in seq_along(base_lst)) {
    Ub = base_lst[[i]]
    prod = t(Uc) %*% Ub
    Mat = Mat + Uc %*% (prod %*% t(prod)) %*% t(Uc)
  }
  Mat = Mat / length(base_lst)
  min( RSpectra::svds(Mat, ncol(Z), 0, 0)$d )
}


all_path_FSSS <- function(X, K, alpha, bags) {
  # X: n x p feature matrix
  # y: n x 1 response variable
  # K: number of models desired
  # alpha: stability threshold in (1/2, 1)
  # B: number of subsamples (even integer)
  # P_avg: p x p average projection matrix
  
  n <- nrow(X)
  p <- ncol(X)
  
  MAX_STABLE <- list()
  VISITED <- list()
  
  # Main loop: build K models
  while (length(MAX_STABLE) < K) {
    cat("looking for the", length(MAX_STABLE) + 1, "th maximal alpha stable model: \n")
  
    S <- integer(0)  # Current selected feature set
  
    # Sequential variable addition
    repeat {
      # Step (2): Find candidate features
      F_candidates <- integer(0)
  
      for (j in setdiff(1:p, S)) {
        # Check if S ∪ {j} is not in MAX_STABLE or VISITED
        S_union_j <- sort(c(S, j))
  
        is_in_max_stable <- any(sapply(MAX_STABLE, function(ms) {
          setequal(S_union_j, ms)
        }))
  
        is_in_visited <- any(sapply(VISITED, function(v) {
          setequal(S_union_j, v)
        }))
  
        if (!is_in_max_stable && !is_in_visited) {
          # Check trace condition
          trace_val <- compute_trace(S, j, bags)
          if (trace_val >= alpha) {
            F_candidates <- c(F_candidates, j)
          }
        }
      }
  
      # Step (3): If no candidates, go to step (4)
      if (length(F_candidates) == 0) break
  
      # Compute weights for each candidate
      weights <- sapply(F_candidates, function(j) {
        trace_val <- compute_trace(S, j, bags)
        indicator <- as.numeric(trace_val >= alpha)
        return(trace_val * indicator)
      })
  
      # Normalize weights
      weights <- weights / sum(weights)
  
      # Select j with probability wj
      if(length(F_candidates) == 1) {
        selected_j = F_candidates[1]
      } else {
        selected_j <- sample(F_candidates, size = 1, prob = weights)
      }
  
      # Step (3a): Check if S ∪ {j} has no super-set in MAX_STABLE
      S_union_j <- sort(c(S, selected_j))
  
      has_superset <- any(sapply(MAX_STABLE, function(ms) {
        all(S_union_j %in% ms) && length(ms) > length(S_union_j)
      }))
  
      if (!has_superset) {
        pi_val <- compute_pi(S_union_j, X, bags)
      } else {
        pi_val <- 1  # Must be >= alpha
      }
  
      # Step (3b): Check stability
      if (pi_val >= alpha) {
        S <- S_union_j
        # Continue to step (2)
      } else {
        # Add to VISITED and continue with other candidates
        VISITED <- c(VISITED, list(S_union_j))
        # Remove selected_j from candidates and repeat step (3)
        F_candidates <- setdiff(F_candidates, selected_j)
        if (length(F_candidates) == 0) break
      }
    }
  
    # Step (4): Process the final S
    if (length(S) == 0) {
      return(NULL)
    }
  
    cat("found a set", "\n")
  
    # Step (4a): Check if S has no super-set in MAX_STABLE
    has_superset <- any(sapply(MAX_STABLE, function(ms) {
      all(S %in% ms) && length(ms) > length(S)
    }))
  
    if (!has_superset) {
      MAX_STABLE <- c(MAX_STABLE, list(S))
      cat("it is maximal alpha stable:", S, "\n")
    } else {
      VISITED <- c(VISITED, list(S))
      cat("it is visited", "\n")
    }
    cat("===========", "\n")
  }
  
  # Step (5): Return MAX_STABLE
  return(MAX_STABLE)
}

