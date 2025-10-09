def challenge(i,j):
    sol = [[0]*j for _ in range(i)]
    sol[0][0] 
    # reverse
    sol[0][1] 
    sol[1][0] 

    # reverse
    sol[0][2] 
    sol[1][1] 
    sol[2][0] 

    # reverse
    sol[0][3] 
    sol[1][2] 
    sol[2][1] 
    sol[3][0] 

    # reverse
    sol[1][3] 
    sol[2][2] 
    sol[3][1] 

    # reverse
    sol[2][3] 
    sol[3][2] 
    
    # reverse
    sol[3][3] 

    return sol

def challenge2(n):
    sol = [[0]*n for _ in range(n)]
    def challenge22(k, last, order, mat):
        if last == n**2:
            return mat
        j = min(k, n-1)
        i = k - j
        nums = list(range(last, last + j - i + 1))
        print(nums)
        ni = 0
        if not order:
            nums = nums[::-1]

        while j - i <= k:
            mat[i][j] = nums[ni]
            j -= 1
            ni +=1
            i += 1
            nprint(mat)
        nprint(mat)
        return challenge22(k + 1, nums[ni], not order, mat)


    return challenge22(0, 1, True, sol)

def nprint(x):
    for i in x:
        print(i)

n = 2 
for j in range(n+1):
    i = n - j
    print(i, j)

nprint(challenge2(4))
