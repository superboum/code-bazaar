Credits: [@halfa](https://github.com/halfa)

```
kubectl apply --validate=false -f 01_cert_manager.yml
kubectl apply -f 02_lets_encrypt.yml
kubectl apply -f 03_cweb_deployment.yml
kubectl apply -f 04_cweb_ingress.yml
