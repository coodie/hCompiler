int fib_it(int n)
{
    int i = n;
    int a = 0;
    int b = 1;
    int res = 0;
    while(i >= 1)
    {
        res = a+b;
        a = b;
        b = res;
        i = i-1;
    }
    return res;
}

int fib_rec(int n)
{
    if(n <= 1)
        return 1;
    else
        return fib_rec(n-1)+fib_rec(n-2);
}

int main()
{
    print_int(fib_it(1));
    print_int(fib_it(2));
    print_int(fib_it(3));
    print_int(fib_it(4));
    print_int(fib_it(5));

    print_int(fib_rec(1));
    print_int(fib_rec(2));
    print_int(fib_rec(3));
    print_int(fib_rec(4));
    print_int(fib_rec(5));
}
