package ca.viu.csci331.lab6;

public class RationalNumber {
    private int numerator;
    private int denominator;
    
    public RationalNumber(int numerator, int denominator) {
        if (denominator == 0) {
            throw new DivideByZeroRuntimeException();
        }
        this.numerator = numerator;
        this.denominator = denominator;
    }
    
    public RationalNumber(RationalNumber other) {
        numerator = other.numerator;
        denominator = other.denominator;
    }
    
    public int getNumerator() {
        return numerator;
    }
    
    public int getDenominator() {
        return denominator;
    }
    
    public void reduce() {
        int comDiv = gcd(numerator, denominator);
        numerator /= comDiv;
        denominator /= comDiv;
    }
    
    public boolean equalFields(RationalNumber other) {
        if (getNumerator() != other.getNumerator()) {
            return false;
        }
        if (getDenominator() != other.getDenominator()) {
            return false;
        }
        return true;
    }
    
    public double toDouble() {
        return (double) getNumerator() / getDenominator();
    }
    
    private int gcd(int x, int y) {
        if (y == 0) {
            return x;
        }
        return gcd(y, x%y);
    }
}
