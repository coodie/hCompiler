int factorial(int n)
{
    if(n <= 1)
        return 1;
    else
        return n*factorial(n-1);
}

int main()
{
    print_int(factorial(10));
    return 0;
}
