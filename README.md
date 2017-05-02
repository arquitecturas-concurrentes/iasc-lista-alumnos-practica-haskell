# Instalacion

```bash
cd alumnos_lista
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal run
```

(in another terminal)

```bash
nc localhost <a_port>
```
