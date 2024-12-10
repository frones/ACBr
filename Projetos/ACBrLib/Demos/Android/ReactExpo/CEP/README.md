ACBrLibCep Android 
----

Exemplo React Native (Expo) do ACBrLibCep


## Importando a ACBrLibCep ##

1. Baixe a ACBrLibCep-debug.aar
2. Copie a ACBrLibCep-debug.aar para android/app/libs

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

# Executando o projeto 

```powershell
npm i
npm start
```

