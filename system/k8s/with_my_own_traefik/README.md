k8s
===

## About

deploy a k8s stack compatible with scaleway kapsule service:
  - a traefik v2 frontend
    - configured to fetch let's encrypt certificates
    - configured to integrate with kubernetes CRD (following [Traefik Kubernetes CRD Doc (Definitions and RBAC sections)](https://docs.traefik.io/reference/dynamic-configuration/kubernetes-crd/))
    - configured as a daemon set (inspired by [wescale blog post](https://blog.wescale.fr/2020/03/06/traefik-2-reverse-proxy-dans-kubernetes/))
    - to bind an externally available service, scaleway seems to require a load balancer, so I put a load balancer in front of traefik
  - a basic backend named cweb
    - automatically register on traefik via Kubernetes CRD (following [Traefik Kubernetes CRD Doc (Resources section)](https://docs.traefik.io/reference/dynamic-configuration/kubernetes-crd/) and [wescale blog post (close to the end)](https://blog.wescale.fr/2020/03/06/traefik-2-reverse-proxy-dans-kubernetes/))
    - should be easy to add as many backends as you want :-)

## Warnings

  - let's encrypt certificates are not persisted and could lead to unoptimal API querying on Let's Encrypt API that, in turn, could lead to API rate limiting
  - admin API is exposed to the internet, in production you may want to at least add a password or even not expose it all...
