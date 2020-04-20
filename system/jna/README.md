```
sudo dnf install jna opus
javac -Xlint:deprecation -classpath /usr/lib/java/jna.jar *.java
java -classpath /usr/lib/java/jna.jar:`pwd` Opus
```
