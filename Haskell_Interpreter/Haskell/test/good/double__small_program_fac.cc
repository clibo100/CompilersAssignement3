int main() {
  printInt(1);
  printDouble(dfac(10.0));
  return 0 ;
}

double dfac(double n)
{
  printInt(2);
  if (n != 0.0)
  {
    dfac(n-1.0);
    printDouble(n);
  }
  else
  {
    printInt(3);
    return n;
  }
}
