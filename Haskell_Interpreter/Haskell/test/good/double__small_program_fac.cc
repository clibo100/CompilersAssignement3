int main() {
  printInt(0);
  printDouble(dfac(10.0));
  return 0 ;
}

double dfac(double n)
{
  double f ;
  printInt(1);
  if (n != 0.0)
  {
    printDouble(n);
    dfac(n - 1.0);
  }
  else
  {
    printInt(2);
  }
  return f ;
}