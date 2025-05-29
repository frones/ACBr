# 🧪 Programa Exemplo ACBrLibBAL Flutter (Android)

Este projeto demonstra a integração de um aplicativo Flutter com a biblioteca nativa ACBrLibBAL. Desenvolvida a partir do componente ACBrBAL do Projeto ACBr, esta biblioteca possibilita a comunicação e controle de balanças de diversas marcas e modelos, permitindo a leitura de pesos. O programa exemplo serve como um guia prático para desenvolvedores que desejam implementar a funcionalidade de integração com balanças em suas aplicações Flutter.

## 🎯 Visão Geral do Projeto Exemplo

Este **Programa Exemplo** foi desenvolvido exclusivamente para fins de **demonstração e estudo**. Ele serve como uma base de referência para desenvolvedores entenderem a integração com a ACBrLibBAL no ambiente Flutter.

⚠️ **Importante:** O código presente neste projeto **NÃO DEVE** ser utilizado diretamente em aplicações reais ou ambientes de produção sem uma revisão completa, refatoração e implementação de práticas de segurança e tratamento de erros adequadas ao seu caso de uso específico.

No demo, você encontrará:

- A **estrutura essencial** de um projeto Flutter para comunicação com funcionalidades nativas.
- Uma seção dedicada a **configurações da biblioteca**, incluindo a exibição do conteúdo do arquivo ``acbrlib.ini``.
- A implementação do padrão **Singleton** para o plugin da ACBrLibBAL, garantindo que apenas uma instância da lib seja utilizada em todo o aplicativo.

## 🚀 Instalação e Execução

Para colocar o programa exemplo em funcionamento, siga os passos abaixo:

1. **Obtenha os Arquivos Necessários da ACBrLibBAL:**
   - Acesse a seção de downloads do fórum oficial do Projeto ACBr:\
      [https://www.projetoacbr.com.br/forum/files/](https://www.projetoacbr.com.br/forum/files/)
   - Selecione **ACBrLibBAL** e, na seção de downloads, escolha a opção para **Android**.
   - Após o download, descompacte o arquivo.
   - Dentro da pasta descompactada, você encontrará a subpasta ``Android`` (contendo o ``.aar``).

2. **Configuração da ACBrLibBAL no Projeto Flutter:**

    - **Arquivo ``.aar``:** Pegue o arquivo ``ACBrLibBAL-release.aar`` (localizado em ``Android`` na pasta que você descompactou) e copie-o para a pasta ``android/app/libs`` do demo. Se a pasta ``libs`` não existir dentro de ``android/app``, crie-a.

Após concluir esta configuração (arquivo ``.aar``), você estará pronto para explorar e utilizar todas as funcionalidades do programa exemplo!

## 📂 Estrutura do Projeto

A organização do código foi pensada para clareza e manutenção, dividindo as responsabilidades em diretórios lógicos:

### ``lib``

- ``plugin/``: Abriga o arquivo ``acbrbal_plugin.dart``. Este é o coração da comunicação com o mundo nativo, servindo como a "ponte" via **Method Channel** para todas as funções da ACBrLibBAL. Cada função neste arquivo está detalhadamente documentada com as mesmas explicações e parâmetros encontrados na documentação oficial da ACBrLib. Para a documentação mais atualizada, consulte diretamente em:\
[https://acbr.sourceforge.io/ACBrLib/SobreaACBrLibBAL.html](https://acbr.sourceforge.io/ACBrLib/SobreaACBrLibBAL.html)

- ``ui/``: Onde estão organizadas as telas (screens) e os widgets da interface. Veja a estrutura detalhada abaixo.

- ``utils/``: Contém arquivos utilitários diversos do projeto. Nela você encontrará o arquivo ``acbrlib_bal_helper.dart``, que implementa a classe **Singleton** para garantir que apenas uma instância da ACBrLibBAL seja utilizada em todo o aplicativo. Esta abordagem pode ser um ótimo ponto de partida para sua própria implementação.

#### ``ui/`` (Estrutura Detalhada da Interface do Usuário)

O diretório ``ui/`` é subdividido para organizar as diferentes partes da interface:

- ``_core/``:
    - ``app_colors.dart``: Definições das cores utilizadas globalmente no tema do aplicativo.
    - ``app_theme.dart``: Configurações do tema visual geral da aplicação.
- ``acbrlib_ini/``: Contém a tela responsável pela exibição do arquivo de configuração ``acbrlib.ini``.
- ``bal/``: Contém uma tela que simula uma balança, onde você pode interagir diretamente com os comandos e funcionalidades da ACBrLibBAL.
- ``configuracoes/``: Contém a tela de configurações da balança.
- ``home/``: Contém a tela principal que organiza a navegação em abas, incluindo as seções: **BAL**, **Configurações**, **ACBrLib.ini**. É nesta tela que ocorre a **inicialização** da ACBrLibBAL.

Todo o código deste projeto exemplo está **bem documentado** por meio de comentários e doc comments detalhados, facilitando a compreensão. Caso surjam dúvidas, sinta-se à vontade para criar um tópico no [fórum oficial do Projeto ACBr](https://www.projetoacbr.com.br/forum/) ou entrar em contato através do [Discord](https://www.projetoacbr.com.br/discord).

## 📝 Resumo e Considerações Finais

Este projeto é um programa exemplo crucial para entender a integração Flutter com a **ACBrLibBAL**. Lembre-se:

- **Não é para Produção:** O código é uma base de estudo e não deve ser usado em produção.
- **Configuração Essencial:** Certifique-se de configurar corretamente o arquivo ``.aar`` no app para ativar as funcionalidades da lib.
- **Comunicação Nativa:** A integração com a ACBrLib é feita via **Method Channel**, uma ponte vital entre o Dart e o código nativo (Java/Kotlin).
- **Documentação:** O projeto está bem documentado com comentários para facilitar a compreensão.


---
**Suporte:** Se tiver qualquer dúvida, sinta-se à vontade para abrir um tópico no [fórum oficial do Projeto ACBr](https://www.projetoacbr.com.br/forum/) ou entrar em contato através do [Discord](https://www.projetoacbr.com.br/discord).
