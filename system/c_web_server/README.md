Compile :

```
gcc -Wall -static main.c -o main
```

Launch with param:

```
HTTP_PORT=8081 DISPLAY_NAME=Mathieu ./main 
```

Query:

```
curl http://127.0.0.1:8080
```

Docker :

```
sudo docker build -t superboum/amd64_cweb .
```

