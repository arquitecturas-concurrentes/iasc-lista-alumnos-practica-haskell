# Práctica de STM

## Dominio

Este dominio, consiste en una lista de preguntas que son contestadas y respondidas por alumnos. Esto se ejecuta bajo un servidor, y el dominio es tan solo que existan canales o secciones y que estos contengan el estado de cada una de las preguntas que existen y de los clientes (alumnos o profesores) que estan conectados a la lista. En este caso cualquiera puede hacer una consulta pero solo los docentes pueden responderla. Para determinar si un usuario es docente o alumno, puede tomarse algun tipo de convencion, como A_Jose para un alumno y P_Juan para un profesor, ya que el objetivo de esta práctica no es el de modelar roles o de que exista un mecanismo de seguridad, pero el que el servidor pueda tener estados que sean sincronizados entre multiples clientes y en un caso futuro, en un ambiente distribuido.

El código de este proyecto esta incompleto o posee errores tanto de sintaxis como de dominio, el objetivo es tratar de aprender como esta armado el servidor y como funcionan los mecanismos de valores sincronizados y mutables (MVars, Chan, STM, TVars).

El proyecto se usa con cabal, y esta listo para que puedan resolverse las dependencias de Haskell que se necesiten. Para la instalacion hay que ejecutar los comandos previos a cabal run. Una vez que hayan instalado las dependencias, solo resta completar el código. Para probar el codigo ejecutar cabal run. Los clientes son en realidad conexiones a traves de nc por socket.

Para instalar cabal ver en https://www.haskell.org/cabal/download.html o usar su manejador de dependencias de linux/unix.

## Instalacion

```bash
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal run
```

(in another terminal)

```bash
nc localhost <a_port>
```
