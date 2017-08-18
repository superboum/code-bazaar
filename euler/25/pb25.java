import java.math.*;

public class pb25{
  
  public static void pb25(int limite){
    BigInteger x = new BigInteger("1");
    BigInteger y = new BigInteger("1");
    BigInteger z = new BigInteger("0");
    BigInteger temp = new BigInteger("0");
    boolean continuer = true;
    int compteur=3;
    while(continuer){
      
      z = x.add(y);
      //System.out.println(z);
      if(z.toString().length()>=limite){
        continuer = false;
        temp = z;
      }
      
      x =y;
      y=z;
      compteur++;
    }
    compteur--;
    System.out.println(compteur);
  }
     
  public static void main(String[] args){
    pb25(1000);
  }
}