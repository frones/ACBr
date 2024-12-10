ACBrLibCep Android 
----

Exemplo React Native (puro) do ACBrLibCep


Executar o projeto com Metro Server
---


## Obs: ##
React Native exige as ferramentas do Android SDK ou uma instalação completa do Android Studio

### Dica do Android Studio: ###

Obtenha o caminho do Android SDK:  Android Studio -> Tools -> SDK Manager
Procure por *Android SDK Location* e copie o caminho!

## Exportando Android SDK ##


### Linux  ###

```bash
export ANDROID_HOME=sdk_path 
```

### Windows ###

Adicione **ANDROID_HOME** as variáveis de ambiente.
Obs: Configure o powershell como shell padrão do Nodejs: 

```powershell
npm config set script-shell "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"

```


#### No powershell #### 

```powershell
 $env:ANDROID_HOME=sdk_path
```


## Inicialize o Metro Server ##

```bash
npm start
```

## Inicie o aplicativo ##

```bash
npm run android
```

Executar o projeto sem o Metro Server
---


```bash
npm run build
```

Executando o projeto
---

```bash
npm run android
```