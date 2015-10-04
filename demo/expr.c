int pow(int a, int n)
{
    int i = 1;
    int res = 1;
    while(i <= n)
    {
        res = res*a;
        i = i+1;
    }
    return res;
}

int main()
{
    int x = 2+17+1*2-pow(2,3);
    int y = x*2 - 2*x + pow(x,3)*3+19+x;
    print_int(x+y);
    return 0;
}



