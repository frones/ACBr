# 🧪 Programa Exemplo ACBrLibNFe Flutter (Android)

Este projeto demonstra a integração de um aplicativo Flutter com a biblioteca nativa **ACBrLibNFe**. Desenvolvida a partir do componente **ACBrNFe** do **Projeto ACBr**, esta biblioteca possibilita a emissão de **Nota Fiscal Eletrônica (NFe)** e **Nota Fiscal Eletrônica do Consumidor (NFCe)**, além de gerenciar todos os eventos relacionados a esses Documentos Fiscais Eletrônicos (DF-e). O programa exemplo serve como um guia prático para desenvolvedores que desejam implementar a funcionalidade de emissão fiscal em suas aplicações Flutter.

## 🎯 Visão Geral do Projeto Exemplo

Este **Programa Exemplo** foi desenvolvido exclusivamente para fins de **demonstração e estudo**. Ele serve como uma base de referência para desenvolvedores entenderem a integração com a ACBrLibNFe no ambiente Flutter.

⚠️ **Importante:** O código presente neste projeto **NÃO DEVE** ser utilizado diretamente em aplicações reais ou ambientes de produção sem uma revisão completa, refatoração e implementação de práticas de segurança e tratamento de erros adequadas ao seu caso de uso específico.

No demo, você encontrará:

- A **estrutura essencial** de um projeto Flutter para comunicação com funcionalidades nativas.
- Um exemplo de **serialização de respostas JSON** para criar e manipular objetos facilmente.
- Uma seção dedicada a **configurações da biblioteca**, incluindo a exibição do conteúdo do arquivo ``acbrlib.ini``.
- A implementação do padrão **Singleton** para o plugin da ACBrLibNFe, garantindo que apenas uma instância da lib seja utilizada em todo o aplicativo.

## 🚀 Instalação e Execução

Para colocar o programa exemplo em funcionamento, siga os passos abaixo:

1. **Obtenha os Arquivos Necessários da ACBrLibNFe:**

    - Acesse a seção de downloads do fórum oficial do Projeto ACBr:\
      [https://www.projetoacbr.com.br/forum/files/](https://www.projetoacbr.com.br/forum/files/)
    - Selecione **ACBrLibNFe** e, na seção de downloads, escolha a opção para **Android**.
    - Após o download, descompacte o arquivo.
    - Dentro da pasta descompactada, você encontrará as subpastas ``Android`` (contendo o ``.aar``) e ``dep`` (com a pasta ``Schemas``).

2. **Configuração da ACBrLibNFe no Projeto Flutter:**

    - **Arquivo ``.aar``:** Pegue o arquivo ``ACBrLibNFe-release.aar`` (localizado em ``Android`` na pasta que você descompactou) e copie-o para a pasta ``android/app/libs`` do demo. Se a pasta ``libs`` não existir dentro de ``android/app``, crie-a.
    - **Pasta ``Schemas``:**
        - Compacte a pasta ``Schemas`` (encontrada dentro da pasta ``dep`` que você descompactou) em um arquivo ZIP com o nome ``schemas.zip`` (em minúsculas e no formato ``.zip``).
        - Mova este arquivo ``schemas.zip`` para a pasta ``assets`` na raiz do demo.

        - **Observação:** Ao iniciar o aplicativo, ele automaticamente descompactará o ``schemas.zip`` para o diretório de arquivos internos do app, com uma validação inteligente para descompactar apenas quando necessário. Esta abordagem pode ser replicada em sua aplicação final, se desejar.
        - **Dica para Android Studio:** Se estiver usando o Android Studio, você pode simplificar este passo. Com o app rodando no emulador ou dispositivo, use o **Device File Explorer (``View > Tool Windows > Device Explorer``)**, navegue até ``/data/data/br.com.projetoacbr.example.acbrlib.nfe.acbrlibnfe/app_flutter`` e simplesmente arraste a pasta ``Schemas`` (descompactada) para lá.

3. **Configuração do Certificado Digital:**

    - Após o aplicativo demo ser iniciado e estar rodando, navegue até a seção de **Configurações** e, em seguida, para a aba de **Certificados**.
    - Nesta tela, você precisará importar um arquivo de **certificado ``.pfx`` válido**. Este certificado é essencial para que a ACBrLibNFe consiga operar todas as suas funcionalidades de emissão fiscal.

Após concluir estas três configurações (arquivo ``.aar``, ``schemas.zip`` ou pasta ``Schemas`` no Device Explorer, e importação do certificado ``.pfx``), você estará pronto para explorar e utilizar todas as funcionalidades do programa exemplo!

## 📂 Estrutura do Projeto

A organização do código foi pensada para clareza e manutenção, dividindo as responsabilidades em diretórios lógicos:

### ``lib``

Este é o diretório principal do código-fonte Dart:

- ``model/``: Contém os modelos de dados do aplicativo. Aqui você encontrará o exemplo ``status_servico.dart``,
  que demonstra como armazenar a resposta do status do serviço e, mais importante, como usar o método ``fromJson`` para serializar respostas JSON em objetos Dart, facilitando a manipulação dos dados recebidos.

- ``plugin/``: Abriga o arquivo ``acbrnfe_plugin.dart``. Este é o coração da comunicação com o mundo nativo, servindo como a "ponte" via **Method Channel** para todas as funções da ACBrLibNFe. Cada função neste arquivo está detalhadamente documentada com as mesmas explicações e parâmetros encontrados na documentação oficial da ACBrLib. Para a documentação mais atualizada, consulte diretamente em:\
  [https://acbr.sourceforge.io/ACBrLib/SobreaACBrLibNFe.html](https://acbr.sourceforge.io/ACBrLib/SobreaACBrLibNFe.html)
- ``ui/``: Onde estão organizadas as telas (screens e pages) e os widgets da interface. Veja a estrutura detalhada abaixo.
- ``utils/``: Contém o arquivo ``utils.dart``, que reúne funções utilitárias diversas do projeto. Notavelmente, é aqui que reside a lógica para importar e descompactar a pasta ``schemas.zip`` no início do aplicativo. Além disso, você encontrará o arquivo ``acbrlib_nfe_helper.dart``, que implementa a classe **Singleton** para garantir que apenas uma instância do plugin seja utilizada em todo o aplicativo. Esta abordagem pode ser um ótimo ponto de partida para sua própria implementação.

#### ``ui/`` (Estrutura Detalhada da Interface do Usuário)

O diretório ``ui/`` é subdividido para organizar as diferentes partes da interface:

- ``_core/``:
    - ``app_colors.dart``: Definições das cores utilizadas globalmente no tema do aplicativo.
    - ``app_theme.dart``: Configurações do tema visual geral da aplicação.
- ``acbrlib_ini/``: Contém a tela responsável pela exibição do arquivo de configuração ``acbrlib.ini``.
- ``configuracoes/``: Armazena as diferentes páginas que compõem as abas do menu de configurações (**Geral**, **WebServices**, **Certificados**, **Arquivos**, **Email** e **Documento Auxiliar**). Além de abrigar a página principal que agrupa essas abas.
- ``home/``: Contém a tela principal que organiza a navegação em abas, incluindo as seções: **NFe**, **Configurações**, **ACBrLib.ini**. É nesta tela que ocorre a **inicialização** da ACBrLibNFe.
- ``nfe/``: Contém as diferentes páginas que compõem as abas do menu de comandos NFe (**Envio**, **Consultas**, **Eventos**, **Inutilização** e **Distribuição DFe**). Além de abrigar a página principal que agrupa essas abas.

Todo o código deste projeto exemplo está **bem documentado** por meio de comentários e doc comments detalhados, facilitando a compreensão. Caso surjam dúvidas, sinta-se à vontade para criar um tópico no [fórum oficial do Projeto ACBr](https://www.projetoacbr.com.br/forum/) ou entrar em contato através do [Discord](https://www.projetoacbr.com.br/discord).

## 🗺️ Futuro (Roadmap)
- [ ] Implementação resposta JSON para os demais métodos.
- [ ] **Integração com Classe de Alto Nível:** Adicionar a opção para que o Flutter possa consumir diretamente a classe de alto nível da Nota Fiscal. Isso oferecerá uma **alternativa** mais simplificada para quem prefere não trabalhar diretamente com arquivos INI ou XML.

## 📝 Resumo e Considerações Finais

Este projeto é um programa exemplo crucial para entender a integração Flutter com a **ACBrLibNFe**. Lembre-se:

- **Não é para Produção:** O código é uma base de estudo e não deve ser usado em produção.
- **Configuração Essencial:** Certifique-se de configurar corretamente o arquivo ``.aar``, a pasta ``schemas.zip`` (ou via Device Explorer) e, principalmente, **importar um certificado** ``.pfx`` no app para ativar as funcionalidades da lib.
- **Comunicação Nativa:** A integração com a ACBrLib é feita via **Method Channel**, uma ponte vital entre o Dart e o código nativo (Java/Kotlin).
- **Documentação:** O projeto está bem documentado com comentários para facilitar a compreensão.
---
**Suporte:** Se tiver qualquer dúvida, sinta-se à vontade para abrir um tópico no [fórum oficial do Projeto ACBr](https://www.projetoacbr.com.br/forum/) ou entrar em contato através do [Discord](https://www.projetoacbr.com.br/discord).
