int main() {
  printInt(0);
  printDouble(dfac(10.0));
  return 0 ;
}

double dfac(double n)
{
  double f ;
  printInt(1);
  if (n == 0.0)
  {
    printInt(2);
    f = 1.0;
  }
  else
  {
    printInt (3);
    f = n * dfac(n-1.0);
  }
  return f ;
}