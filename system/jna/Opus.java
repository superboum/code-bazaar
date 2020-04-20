import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Memory;

/* Some help :
 *  - https://github.com/xiph/opus/blob/master/include/opus.h
 *  - https://github.com/xiph/opus/blob/master/include/opus_defines.h
 *  - https://opus-codec.org/docs/opus_api-1.2/index.html
 *  - https://java-native-access.github.io/jna/4.2.1/com/sun/jna/Pointer.html
 */
public class Opus {
  public interface NativeOpus extends Library {
    public static int OPUS_APPLICATION_VOIP = 2048;
    public static int OPUS_GET_APPLICATION = 4001;

    public Pointer opus_encoder_create(int Fs, int channels, int application, Pointer error);
    public int opus_encoder_ctl(Pointer opus_enc, int req, Pointer param);
  }

  public static void main(String []args) {
    // "opus" = "libopus.so" on linux
    NativeOpus no = (NativeOpus) Native.load("opus", NativeOpus.class);

    Pointer oe =  new Memory(32/8); // an int 32
    Pointer opus_enc = no.opus_encoder_create(48000, 2, NativeOpus.OPUS_APPLICATION_VOIP, oe);

    Pointer answer = new Memory(32/8); // an int 32
    no.opus_encoder_ctl(opus_enc, NativeOpus.OPUS_GET_APPLICATION, answer);

    System.out.println("encoder application is set to "+answer.getInt(0)+" (expecting: " + NativeOpus.OPUS_APPLICATION_VOIP + ")");
  }
}
