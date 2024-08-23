<!--
// {******************************************************************************}
// { Projeto: Componentes ACBr                                                    }
// {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
// { mentos de Automação Comercial utilizados no Brasil                           }
// {                                                                              }
// { Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
// {                                                                              }
// { Colaboradores nesse arquivo: Renato Rubinho                                  }
// {                                                                              }
// {  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
// { Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
// {                                                                              }
// {  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
// { sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
// { Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
// { qualquer versão posterior.                                                   }
// {                                                                              }
// {  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
// { NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
// { ADEQUAÇÃO A UMA FINALIDADE ESPECÝFICA. Consulte a Licença Pública Geral Menor}
// { do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
// {                                                                              }
// {  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
// { com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
// { no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
// { Você também pode obter uma copia da licença em:                              }
// { http://www.opensource.org/licenses/lgpl-license.php                          }
// {                                                                              }
// { Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
// {       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
// {******************************************************************************}
-->

<!DOCTYPE html>

<head>
    <meta charset="UTF-8">
    <meta name="ACBrNFe" content="width=device-width, initial-scale=1.0">
    <title>
        <?php
        $title = 'ACBrNFe';
        if (isset($_GET['modo']))
            $titleModo = $_GET['modo'];
        else
            $titleModo = 'MT';

        if ($titleModo == 'MT') {
            $title .= ' - MultiThread';
        } else {
            $title .= ' - SingleThread';
        }

        echo $title;
        ?>
    </title>
    <style>
        body {
            display: flex;
            margin: 0;
            padding: 0;
            font-family: Arial, sans-serif;
            height: 100vh;
        }

        .container {
            display: flex;
            margin: 20px;
        }

        .cfgPanelEsquerda,
        .cfgPanelDireita {
            width: 50%;
            padding: 10px;
            box-sizing: border-box;
        }

        .cfgPanelEsquerda {
            display: flex;
            flex-direction: column;
            height: 100%;
        }

        .cfgPanelEsquerda .tabsPanel {
            flex-grow: 1;
            overflow-y: auto;
        }

        .cfgPanelEsquerda .buttons {
            text-align: center;
            margin-top: 10px;
            margin-bottom: 10px;
        }

        .cfgPanelEsquerda .buttons button {
            padding: 10px 20px;
            margin: 0 5px;
        }

        .cfgPanelDireita {
            display: flex;
            flex-direction: column;
            height: 100%;
        }

        .cfgPanelDireita .tabsPanel {
            flex: 1;
            border: 1px solid #ccc;
            border-radius: 4px;
            height: 50%;
        }

        .tabAbas {
            display: flex;
        }

        .tabAbas button {
            flex: 1;
            padding: 10px;
            border: 1px solid #ccc;
            background: #f1f1f1;
            cursor: pointer;
        }

        .tabAbas button.selecionado {
            background: #ddd;
        }

        .panelAba {
            border-top: none;
            padding: 10px;
            display: none;
            height: calc(100% - 40px);
            overflow-y: auto;
        }

        .panelAba.selecionado {
            display: block;
        }

        .form-group {
            margin-bottom: 10px;
        }

        .form-group label {
            display: block;
        }

        .form-group input,
        .form-group select {
            width: 100%;
            padding: 5px;
            box-sizing: border-box;
        }

        .response-memo {
            height: 50%;
            margin-top: 10px;
            width: 100%;
            box-sizing: border-box;
        }

        .button-container {
            display: none;
        }

        .button-container.selecionado {
            display: block;
        }

        .panelEsquerda-container {
            display: none;
        }

        .panelEsquerda-container.selecionado {
            display: block;
        }

        .panelConfiguracoes-container {
            display: none;
        }

        .panelConfiguracoes-container.selecionado {
            display: block;
        }

        .grid1Col {
            display: grid;
            grid-template-columns: repeat(1, 1fr);
            gap: 10px;
            row-gap: 1px;
        }

        .grid2Col {
            display: grid;
            grid-template-columns: repeat(2, 1fr);
            gap: 10px;
            row-gap: 1px;
        }

        .grid3Col {
            display: grid;
            grid-template-columns: repeat(3, 1fr);
            gap: 10px;
            row-gap: 1px;
        }

        .grid4Col {
            display: grid;
            grid-template-columns: repeat(4, 1fr);
            gap: 10px;
            row-gap: 1px;
        }
    </style>
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
</head>

<body>

    <div class="cfgPanelEsquerda">
        <div class="tabsPanel">
            <div class="tabAbas">
                <button class="selecionado" onclick="ativaAba(event, 'configuracoes', 'panelEsquerda')">Configurações</button>
                <button onclick="ativaAba(event, 'documento-auxiliar', 'panelEsquerda')">Documento Auxiliar</button>
                <button onclick="ativaAba(event, 'email', 'panelEsquerda')">Email</button>
            </div>
            <div id="configuracoes" class="panelAba selecionado panelEsquerda-container">
                <div class="tabsPanel">
                    <div class="tabAbas">
                        <button class="selecionado" onclick="ativaAba(event, 'geral', 'panelConfiguracoes')">Geral</button>
                        <button onclick="ativaAba(event, 'webservices', 'panelConfiguracoes')">WebServices</button>
                        <button onclick="ativaAba(event, 'certificados', 'panelConfiguracoes')">Certificados</button>
                        <button onclick="ativaAba(event, 'arquivos', 'panelConfiguracoes')">Arquivos</button>
                    </div>
                    <div id="geral" class="panelAba selecionado panelConfiguracoes-container">
                        <div class="grid2Col">
                            <label for="atualizarXml">Atualizar XML</label>
                            <input type="checkbox" id="atualizarXml">
                        </div>
                        <div class="grid2Col">
                            <label for="exibirErroSchema">Exibir Erro Schema</label>
                            <input type="checkbox" id="exibirErroSchema">
                        </div>
                        <div class="form-group">
                            <label for="formatoAlerta">Formato Alerta</label>
                            <input type="text" id="formatoAlerta">
                        </div>
                        <div class="form-group">
                            <label for="formaEmissao">Forma de Emissão</label>
                            <select id="formaEmissao" name="formaEmissao">
                                <option value="0" selected>teNormal</option>
                                <option value="1">teContingencia</option>
                                <option value="2">teSCAN</option>
                                <option value="3">teDPEC</option>
                                <option value="4">teFSDA</option>
                                <option value="5">teSVCAN</option>
                                <option value="6">teSVCRS</option>
                                <option value="7">teSVCSP</option>
                                <option value="8">teOffLine</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label for="modeloDF">Modelo Documento Fiscal</label>
                            <select id="modeloDF" name="modeloDF">
                                <option value="0" selected>moNFe</option>
                                <option value="1">moNFCe</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label for="versaoDF">Versão Documento Fiscal</label>
                            <select id="versaoDF" name="versaoDF">
                                <option value="0">ve200</option>
                                <option value="1">ve300</option>
                                <option value="2">ve310</option>
                                <option value="3" selected>ve400</option>
                            </select>
                        </div>
                        <div class="grid2Col">
                            <label for="retirarAcentos">Retirar Acentos dos XMLs enviados</label>
                            <input type="checkbox" id="retirarAcentos">
                        </div>
                        <div class="grid2Col">
                            <label for="SalvarGer">Salvar Arquivos de Envio e Resposta</label>
                            <input type="checkbox" id="SalvarGer">
                        </div>
                        <div class="form-group">
                            <label for="pathSalvar">Pasta dos Logs</label>
                            <input type="text" id="pathSalvar">
                        </div>
                        <div class="form-group">
                            <label for="pathSchemas">Pasta dos Schemas</label>
                            <input type="text" id="pathSchemas">
                        </div>
                        <div class="form-group">
                            <label for="idCSRT">IdToken/IdCSC (Somente para NFC-e)</label>
                            <input type="text" id="idCSRT">
                        </div>
                        <div class="form-group">
                            <label for="CSRT">Token/CSC (Somente para NFC-e)</label>
                            <input type="text" id="CSRT">
                        </div>
                    </div>
                    <div id="webservices" class="panelAba panelConfiguracoes-container">
                        <div class="grid3Col">
                            <div class="form-group">
                                <label for="UF">Uf Destino</label>
                                <select id="UF">
                                    <option value="AC">AC</option>
                                    <option value="AL">AL</option>
                                    <option value="AP">AP</option>
                                    <option value="AM">AM</option>
                                    <option value="BA">BA</option>
                                    <option value="CE">CE</option>
                                    <option value="DF">DF</option>
                                    <option value="ES">ES</option>
                                    <option value="GO">GO</option>
                                    <option value="MA">MA</option>
                                    <option value="MT">MT</option>
                                    <option value="MS">MS</option>
                                    <option value="MG">MG</option>
                                    <option value="PA">PA</option>
                                    <option value="PB">PB</option>
                                    <option value="PR">PR</option>
                                    <option value="PE">PE</option>
                                    <option value="PI">PI</option>
                                    <option value="RJ">RJ</option>
                                    <option value="RN">RN</option>
                                    <option value="RS">RS</option>
                                    <option value="RO">RO</option>
                                    <option value="RR">RR</option>
                                    <option value="SC">SC</option>
                                    <option value="SP" selected>SP</option>
                                    <option value="SE">SE</option>
                                    <option value="TO">TO</option>
                                </select>
                            </div>
                            <div class="form-group">
                                <label for="SSLType">SSL Type</label>
                                <select id="SSLType">
                                    <option value="0">LT_all</option>
                                    <option value="1">LT_SSLv2</option>
                                    <option value="2">LT_SSLv3</option>
                                    <option value="3">LT_TLSv1</option>
                                    <option value="4">LT_TLSv1_1</option>
                                    <option value="5" selected>LT_TLSv1_2</option>
                                    <option value="6">LT_SSHv2</option>
                                </select>
                            </div>
                            <div class="form-group">
                                <label for="timeout">Timeout</label>
                                <input type="number" id="timeout" value="30000">
                            </div>
                        </div>
                        <div class="grid2Col">
                            <label>Ambiente</label>
                            <div>
                                <input type="radio" id="producao" name="ambiente" value="0" checked>
                                <label for="producao">Produção</label>
                                <input type="radio" id="homologacao" name="ambiente" value="1">
                                <label for="homologacao">Homologação</label>
                            </div>
                        </div>
                        <div class="grid3Col">
                            <label for="visualizar">Visualizar Mensagem</label>
                            <input type="checkbox" id="visualizar">
                        </div>
                        <div class="grid3Col">
                            <label for="SalvarWS">Salvar envelope SOAP</label>
                            <input type="checkbox" id="SalvarWS">
                        </div>
                        <br>
                        <div class="form-group">
                            <label>Retorno de Envio</label>
                            <div class="grid3Col">
                                <label for="ajustaAguardaConsultaRet">Ajustar Auto Aguardar</label>
                                <input type="checkbox" id="ajustaAguardaConsultaRet">
                            </div>
                            <div class="grid3Col">
                                <div>
                                    <label for="aguardarConsultaRet">Aguardar</label>
                                    <input type="number" id="aguardarConsultaRet" value="0">
                                </div>
                                <div>
                                    <label for="tentativas">Tentativas</label>
                                    <input type="number" id="tentativas" value="0">
                                </div>
                                <div>
                                    <label for="intervaloTentativas">Intervalo</label>
                                    <input type="number" id="intervaloTentativas" value="0">
                                </div>
                            </div>
                        </div>
                        <div class="form-group">
                            <label>Proxy</label>
                            <div>
                                <label for="proxyServidor">Servidor</label>
                                <input type="text" id="proxyServidor">
                            </div>
                            <div>
                                <label for="proxyPorta">Porta</label>
                                <input type="number" id="proxyPorta" value="5000">
                            </div>
                            <div>
                                <label for="proxyUsuario">Usuário</label>
                                <input type="text" id="proxyUsuario">
                            </div>
                            <div>
                                <label for="proxySenha">Senha</label>
                                <input type="password" id="proxySenha">
                            </div>
                        </div>
                    </div>
                    <div id="certificados" class="panelAba panelConfiguracoes-container">
                        <div class="form-group">
                            <label for="SSLCryptLib">CryptLib</label>
                            <select id="SSLCryptLib">
                                <option value="0">cryNone</option>
                                <option value="1">cryOpenSSL</option>
                                <option value="2">cryCapicom</option>
                                <option value="3" selected>cryWinCrypt</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label for="SSLHttpLib">HttpLib</label>
                            <select id="SSLHttpLib">
                                <option value="0">httpNone</option>
                                <option value="1">httpWinINet</option>
                                <option value="2" selected>httpWinHttp</option>
                                <option value="3">httpOpenSSL</option>
                                <option value="4">httpIndy</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label for="SSLXmlSignLib">XmlSignLib</label>
                            <select id="SSLXmlSignLib">
                                <option value="0">xsNone</option>
                                <option value="1">xsXmlSec</option>
                                <option value="2">xsMsXml</option>
                                <option value="3">xsMsXmlCapicom</option>
                                <option value="4" selected>xsLibXml2</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label>Certificados</label>
                            <div>
                                <label for="ArquivoPFX">Arquivo PFX</label>
                                <input type="text" id="ArquivoPFX">
                            </div>
                            <div>
                                <label for="DadosPFX">Dados PFX</label>
                                <input type="text" id="DadosPFX">
                            </div>
                            <div>
                                <label for="senhaCertificado">Senha</label>
                                <input type="password" id="senhaCertificado">
                            </div>
                            <div>
                                <label for="NumeroSerie">Número de Série</label>
                                <input type="text" id="NumeroSerie">
                            </div>
                            <br>
                            <div class="grid3Col">
                                <input type="button" id="OpenSSLInfo" value="OpenSSLInfo">
                            </div>
                        </div>
                    </div>
                    <div id="arquivos" class="panelAba panelConfiguracoes-container">
                        <div class="grid2Col">
                            <label for="SalvarArq">Salvar Arquivos em Pastas Separadas</label>
                            <input type="checkbox" id="SalvarArq">
                        </div>
                        <div class="grid2Col">
                            <label for="SepararPorMes">Criar Pastas Mensalmente</label>
                            <input type="checkbox" id="SepararPorMes">
                        </div>
                        <div class="grid2Col">
                            <label for="AdicionarLiteral">Adicionar Literal no nome das pastas</label>
                            <input type="checkbox" id="AdicionarLiteral">
                        </div>
                        <div class="grid2Col">
                            <label for="EmissaoPathNFe">Salvar Doc. pelo campo Data de Emissão</label>
                            <input type="checkbox" id="EmissaoPathNFe">
                        </div>
                        <div class="grid2Col">
                            <label for="SalvarEvento">Salvar Arquivos de Eventos</label>
                            <input type="checkbox" id="SalvarEvento">
                        </div>
                        <div class="grid2Col">
                            <label for="SepararPorCNPJ">Separar Arqs pelo CNPJ do Certificado</label>
                            <input type="checkbox" id="SepararPorCNPJ">
                        </div>
                        <div class="grid2Col">
                            <label for="SepararPorModelo">Separar Arqs pelo Modelo do Documento</label>
                            <input type="checkbox" id="SepararPorModelo">
                        </div>
                        <br>
                        <div class="form-group">
                            <label for="PathNFe">Pasta Arquivos NFe</label>
                            <input type="text" id="PathNFe">
                        </div>
                        <div class="form-group">
                            <label for="PathInu">Pasta Arquivos Inutilização</label>
                            <input type="text" id="PathInu">
                        </div>
                        <div class="form-group">
                            <label for="PathEvento">Pasta Arquivos Evento</label>
                            <input type="text" id="PathEvento">
                        </div>
                    </div>
                </div>
            </div>
            <div id="documento-auxiliar" class="panelAba panelEsquerda-container">
                <div class="form-group">
                    <label for="PathLogo">Logomarca</label>
                    <input type="text" id="PathLogo">
                </div>
                <div class="grid2Col">
                    <label>DANFe</label>
                    <div>
                        <input type="radio" id="danfeRetrato" name="TipoDANFE" value="0" checked>
                        <label for="danfeRetrato">Retrato</label>
                        <input type="radio" id="danfePaisagem" name="TipoDANFE" value="1">
                        <label for="danfePaisagem">Paisagem</label>
                    </div>
                </div>
                <div class="grid2Col">
                    <label>DANFCe</label>
                    <div>
                        <input type="radio" id="danfceFortes" name="TipoRelatorioBobina" value="0" checked>
                        <label for="danfceFortes">Fortes</label>
                        <input type="radio" id="danfceEscPos" name="TipoRelatorioBobina" value="1">
                        <label for="danfceEscPos">EscPos</label>
                        <input type="radio" id="danfceFortesA4" name="TipoRelatorioBobina" value="2">
                        <label for="danfceFortesA4">Fortes A4</label>
                    </div>
                </div>
                <br>
                <div class="form-group">
                    <label>Pos Printer</label>
                    <div class="grid2Col">
                        <div class="form-group">
                            <label for="PosPrinterModelo">Modelo</label>
                            <select id="PosPrinterModelo">
                                <option value="0" selected>ppTexto</option>
                                <option value="1">ppEscPosEpson</option>
                                <option value="2">ppEscBematec</option>
                                <option value="3">ppEscDaruma</option>
                                <option value="4">ppEscVox</option>
                                <option value="5">ppEscDiebold</option>
                                <option value="6">ppEscEpsonP2</option>
                                <option value="7">ppCustomPos</option>
                                <option value="8">ppEscPosStar</option>
                                <option value="9">ppEscZJiang</option>
                                <option value="10">ppEscGPrinter</option>
                                <option value="11">ppEscDatecs</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label for="PaginaDeCodigo">Pag. Código</label>
                            <select id="PaginaDeCodigo">
                                <option value="0">pcNone</option>
                                <option value="1">pc437</option>
                                <option value="2" selected>pc850</option>
                                <option value="3">pc852</option>
                                <option value="4">pc860</option>
                                <option value="5">pcUTF8</option>
                                <option value="6">pc1252</option>
                            </select>
                        </div>
                    </div>
                    <div class="form-group">
                        <label for="PosPrinterPorta">Porta</label>
                        <input type="text" id="PosPrinterPorta">
                    </div>
                    <div class="grid4Col">
                        <div>
                            <label for="ColunasFonteNormal">Colunas</label>
                            <input type="number" id="ColunasFonteNormal" value="0">
                        </div>
                        <div>
                            <label for="EspacoEntreLinhas">Espaços</label>
                            <input type="number" id="EspacoEntreLinhas" value="0">
                        </div>
                        <div>
                            <label for="LinhasBuffer">Buffer</label>
                            <input type="number" id="LinhasBuffer" value="0">
                        </div>
                        <div>
                            <label for="LinhasEntreCupons">Linhas Pular</label>
                            <input type="number" id="LinhasEntreCupons" value="0">
                        </div>
                    </div>
                    <br>
                    <div class="grid3Col">
                        <label for="ControlePorta">Controle Porta</label>
                        <input type="checkbox" id="ControlePorta">
                    </div>
                    <div class="grid3Col">
                        <label for="TraduzirTags">Traduzir Tags</label>
                        <input type="checkbox" id="TraduzirTags">
                    </div>
                    <div class="grid3Col">
                        <label for="CortaPapel">Cortar Papel</label>
                        <input type="checkbox" id="CortaPapel">
                    </div>
                    <div class="grid3Col">
                        <label for="IgnorarTags">Ignorar Tags</label>
                        <input type="checkbox" id="IgnorarTags">
                    </div>
                </div>
            </div>
            <div id="email" class="panelAba panelEsquerda-container">
                <div class="form-group">
                    <label>Configurações</label>
                    <div class="form-group">
                        <label for="emailNome">Nome</label>
                        <input type="text" id="emailNome">
                    </div>
                    <div class="form-group">
                        <label for="emailConta">Email</label>
                        <input type="text" id="emailConta">
                    </div>
                    <div class="form-group">
                        <label for="emailServidor">Host SMTP</label>
                        <input type="text" id="emailServidor">
                    </div>
                    <div class="form-group">
                        <label for="emailPorta">Porta</label>
                        <input type="number" id="emailPorta">
                    </div>
                    <div class="grid4Col">
                        <label for="emailSSL">SSL</label>
                        <input type="checkbox" id="emailSSL">
                    </div>
                    <div class="grid4Col">
                        <label for="emailTLS">TLS</label>
                        <input type="checkbox" id="emailTLS">
                    </div>
                    <div class="form-group">
                        <label for="emailUsuario">Usuário</label>
                        <input type="text" id="emailUsuario">
                    </div>
                    <div class="form-group">
                        <label for="emailSenha">Senha</label>
                        <input type="password" id="emailSenha">
                    </div>
                    <div class="form-group">
                        <label for="emailAssunto">Assunto</label>
                        <input type="text" id="emailAssunto">
                    </div>
                    <div class="form-group">
                        <label for="emailMensagem">Mensagem</label>
                        <input type="text" id="emailMensagem">
                    </div>
                </div>
            </div>
        </div>
        <div class="buttons">
            <input type="button" id="carregarConfiguracoes" value="Carregar Configurações">
            <input type="button" id="salvarConfiguracoes" value="Salvar Configurações">
        </div>
    </div>
    <div class="cfgPanelDireita">
        <div class="tabsPanel">
            <div class="tabAbas">
                <button class="selecionado" onclick="ativaAba(event, 'envio', 'panelDireita')">Envio</button>
                <button onclick="ativaAba(event, 'consultas', 'panelDireita')">Consultas</button>
                <button onclick="ativaAba(event, 'eventos', 'panelDireita')">Eventos</button>
                <button onclick="ativaAba(event, 'inutilizacao', 'panelDireita')">Inutilização</button>
                <button onclick="ativaAba(event, 'distribuicao-dfe', 'panelDireita')">Distribuição DFe</button>
            </div>
            <div id="envio" class="panelAba selecionado button-container">
                <div class="grid3Col">
                    <input type="button" id="EnviarSincronoXml" value="Enviar Sincrono (xml)">
                    <input type="button" id="EnviarAssincronoXml" value="Enviar Assincrono (xml)">
                    <input type="button" id="ImprimirPDF" value="Imprimir PDF DANFe">
                    <input type="button" id="EnviarSincronoIni" value="Enviar Sincrono (ini)">
                    <input type="button" id="EnviarAssincronoIni" value="Enviar Assincrono (ini)">
                    <input type="button" id="SalvarPDF" value="Salvar PDF (Stream)">
                    <input type="button" id="ValidarRegrasdeNegocios" value="Val. Regra de Neg.">
                    <input type="button" id="GerarChave" value="Gerar Chave NFe">
                    <input type="button" id="EnviarEmail" value="Enviar NFe Email">
                </div>
            </div>
            <div id="consultas" class="panelAba button-container">
                <div class="grid3Col">
                    <input type="button" id="statusServico" value="Status de Serviço">
                    <input type="button" id="ConsultarComChave" value="Consultar com Chave">
                    <input type="button" id="ConsultarRecibo" value="Consultar Recibo">
                    <input type="button" id="ConsultaCadastro" value="Consultar Cadastro">
                </div>
            </div>
            <div id="eventos" class="panelAba button-container">
                <div class="grid2Col">
                    <input type="button" id="Cancelar" value="Cancelar NFe">
                    <input type="button" id="EnviarEvento" value="Enviar Evento">
                    <input type="button" id="ImprimirEventoPDF" value="Imprimir PDF Evento">
                    <input type="button" id="SalvarEventoPDF" value="Salvar PDF Evento">
                    <input type="button" id="EnviarEmailEvento" value="Enviar Evento Email">
                </div>
            </div>
            <div id="inutilizacao" class="panelAba button-container">
                <div class="grid3Col">
                    <input type="button" id="Inutilizar" value="Inutilizar Numeração">
                    <input type="button" id="ImprimirInutilizacaoPDF" value="Imprimir PDF de Inutilização">
                    <input type="button" id="SalvarInutilizacaoPDF" value="Salvar PDF de Inutilização">
                </div>
            </div>
            <div id="distribuicao-dfe" class="panelAba button-container">
                <div class="grid3Col">
                    <input type="button" id="DistribuicaoDFePorChave" value="Por Chave">
                    <input type="button" id="DistribuicaoDFePorNSU" value="Por NSU">
                    <input type="button" id="DistribuicaoDFePorUltNSU" value="Por Ult. NSU">
                </div>
            </div>
        </div>
        <div class="grid1Col">
            <label for="result">Respostas</label>
            <textarea id="result" rows="25" readonly></textarea>
        </div>
    </div>

    <?php
    $modo = isset($_GET['modo']) ? $_GET['modo'] : null;
    ?>

    <script>
        function ativaAba(event, abaId, panel) {
            const listaAbas = document.querySelectorAll(`#${panel} .panelAba`);
            listaAbas.forEach(abaItem => {
                abaItem.classList.remove('selecionado');
            });

            document.getElementById(abaId).classList.add('selecionado');

            const outrasAbas = event.target.parentElement.children;
            for (let item of outrasAbas) {
                item.classList.remove('selecionado');
            }

            event.target.classList.add('selecionado');

            // Controle panel Direita
            if (panel === 'panelDireita') {
                const abaSelecionada = document.querySelector(`#${abaId}.button-container`);
                const outrasAbas = document.querySelectorAll('.button-container');
                outrasAbas.forEach(container => container.classList.remove('selecionado'));

                if (abaSelecionada) {
                    abaSelecionada.classList.add('selecionado');
                }
            }

            // Controle panel Esquerda
            if (panel === 'panelEsquerda') {
                const abaSelecionada = document.querySelector(`#${abaId}.panelEsquerda-container`);
                const outrasAbas = document.querySelectorAll('.panelEsquerda-container');
                outrasAbas.forEach(container => container.classList.remove('selecionado'));

                if (abaSelecionada) {
                    abaSelecionada.classList.add('selecionado');
                }
            }

            // Controle panel Configurações
            if (panel === 'panelConfiguracoes') {
                const abaSelecionada = document.querySelector(`#${abaId}.panelConfiguracoes-container`);
                const outrasAbas = document.querySelectorAll('.panelConfiguracoes-container');
                outrasAbas.forEach(container => container.classList.remove('selecionado'));

                if (abaSelecionada) {
                    abaSelecionada.classList.add('selecionado');
                }
            }
        }

        // Inicializa biblioteca
        chamaAjaxEnviar({
            metodo: "carregarConfiguracoes"
        });

        // Chamada do botão para carregar configurações
        $('#carregarConfiguracoes').on('click', function() {
            chamaAjaxEnviar({
                metodo: "carregarConfiguracoes"
            });
        });

        // Chamada do botão para salvar configurações
        $('#salvarConfiguracoes').on('click', function() {
            const infoData = {
                metodo: "salvarConfiguracoes",
                atualizarXml: $('#atualizarXml').prop('checked') ? 1 : 0,
                exibirErroSchema: $('#exibirErroSchema').prop('checked') ? 1 : 0,
                formatoAlerta: $('#formatoAlerta').val(),
                formaEmissao: $('#formaEmissao').val(),
                modeloDF: $('#modeloDF').val(),
                versaoDF: $('#versaoDF').val(),
                retirarAcentos: $('#retirarAcentos').prop('checked') ? 1 : 0,
                SalvarGer: $('#SalvarGer').prop('checked') ? 1 : 0,
                pathSalvar: $('#pathSalvar').val(),
                pathSchemas: $('#pathSchemas').val(),
                idCSRT: $('#idCSRT').val(),
                CSRT: $('#CSRT').val(),
                SSLType: $('#SSLType').val(),
                timeout: $('#timeout').val(),
                ambiente: parseInt($('input[name="ambiente"]:checked').val(), 10),
                visualizar: $('#visualizar').prop('checked') ? 1 : 0,
                SalvarWS: $('#SalvarWS').prop('checked') ? 1 : 0,
                ajustaAguardaConsultaRet: $('#ajustaAguardaConsultaRet').prop('checked') ? 1 : 0,
                aguardarConsultaRet: $('#aguardarConsultaRet').val(),
                tentativas: $('#tentativas').val(),
                intervaloTentativas: $('#intervaloTentativas').val(),
                SalvarArq: $('#SalvarArq').prop('checked') ? 1 : 0,
                SepararPorMes: $('#SepararPorMes').prop('checked') ? 1 : 0,
                AdicionarLiteral: $('#AdicionarLiteral').prop('checked') ? 1 : 0,
                EmissaoPathNFe: $('#EmissaoPathNFe').prop('checked') ? 1 : 0,
                SalvarEvento: $('#SalvarEvento').prop('checked') ? 1 : 0,
                SepararPorCNPJ: $('#SepararPorCNPJ').prop('checked') ? 1 : 0,
                SepararPorModelo: $('#SepararPorModelo').prop('checked') ? 1 : 0,
                PathNFe: $('#PathNFe').val(),
                PathInu: $('#PathInu').val(),
                PathEvento: $('#PathEvento').val(),

                proxyServidor: $('#proxyServidor').val(),
                proxyPorta: $('#proxyPorta').val(),
                proxyUsuario: $('#proxyUsuario').val(),
                proxySenha: $('#proxySenha').val(),

                UF: $('#UF').val(),
                SSLCryptLib: $('#SSLCryptLib').val(),
                SSLHttpLib: $('#SSLHttpLib').val(),
                SSLXmlSignLib: $('#SSLXmlSignLib').val(),
                ArquivoPFX: $('#ArquivoPFX').val(),
                DadosPFX: $('#DadosPFX').val(),
                senhaCertificado: $('#senhaCertificado').val(),
                NumeroSerie: $('#NumeroSerie').val(),

                PathLogo: $('#PathLogo').val(),
                TipoDANFE: parseInt($('input[name="TipoDANFE"]:checked').val(), 10),

                TipoRelatorioBobina: parseInt($('input[name="TipoRelatorioBobina"]:checked').val(), 10),

                PosPrinterModelo: $('#PosPrinterModelo').val(),
                PaginaDeCodigo: $('#PaginaDeCodigo').val(),
                PosPrinterPorta: $('#PosPrinterPorta').val(),
                ColunasFonteNormal: $('#ColunasFonteNormal').val(),
                EspacoEntreLinhas: $('#EspacoEntreLinhas').val(),
                LinhasBuffer: $('#LinhasBuffer').val(),
                LinhasEntreCupons: $('#LinhasEntreCupons').val(),
                ControlePorta: $('#ControlePorta').prop('checked') ? 1 : 0,
                TraduzirTags: $('#TraduzirTags').prop('checked') ? 1 : 0,
                CortaPapel: $('#CortaPapel').prop('checked') ? 1 : 0,
                IgnorarTags: $('#IgnorarTags').prop('checked') ? 1 : 0,

                emailNome: $('#emailNome').val(),
                emailConta: $('#emailConta').val(),
                emailServidor: $('#emailServidor').val(),
                emailPorta: $('#emailPorta').val(),
                emailSSL: $('#emailSSL').prop('checked') ? 1 : 0,
                emailTLS: $('#emailTLS').prop('checked') ? 1 : 0,
                emailUsuario: $('#emailUsuario').val(),
                emailSenha: $('#emailSenha').val()
            };

            chamaAjaxEnviar(infoData);
        });

        // Chamada do botão de status do serviço
        $('#statusServico').on('click', function() {
            chamaAjaxEnviar({
                metodo: "statusServico"
            });
        });

        $('#OpenSSLInfo').on('click', function() {
            chamaAjaxEnviar({
                metodo: "OpenSSLInfo"
            });
        });

        $('#ValidarRegrasdeNegocios').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXmlNFe) {
                chamaAjaxEnviar({
                    metodo: "ValidarRegrasdeNegocios",
                    AeArquivoXmlNFe: AeArquivoXmlNFe
                });
            });
        });

        $('#GerarChave').on('click', function() {
            inputBox("Digite o Código da UF:", function(ACodigoUF) {
                inputBox("Digite o Código numérico:", function(ACodigoNumerico) {
                    inputBox("Digite o Modelo:", function(AModelo) {
                        inputBox("Digite a Série:", function(ASerie) {
                            inputBox("Digite o Número:", function(ANumero) {
                                inputBox("Digite o Tipo de Emissão:", function(ATpEmi) {
                                    inputBox("Digite a Data de Emissão(dd/mm/aaaa):", function(AEmissao) {
                                        inputBox("Digite o CNPJ:", function(ACNPJCPF) {
                                            chamaAjaxEnviar({
                                                metodo: "GerarChave",
                                                ACodigoUF: ACodigoUF,
                                                ACodigoNumerico: ACodigoNumerico,
                                                AModelo: AModelo,
                                                ASerie: ASerie,
                                                ANumero: ANumero,
                                                ATpEmi: ATpEmi,
                                                AEmissao: AEmissao,
                                                ACNPJCPF: ACNPJCPF
                                            });
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });

        $('#ConsultarComChave').on('click', function() {
            inputBox("Digite a chave da NFe:", function(eChaveOuNFe) {
                chamaAjaxEnviar({
                    metodo: "Consultar",
                    eChaveOuNFe: eChaveOuNFe,
                    AExtrairEventos: 1
                });
            });
        });

        $('#Inutilizar').on('click', function() {
            inputBox("Digite o CNPJ:", function(ACNPJ) {
                inputBox("Digite a Justificativa:", function(AJustificativa) {
                    inputBox("Digite o Ano:", function(AAno) {
                        inputBox("Digite o Modelo:", function(AModelo) {
                            inputBox("Digite a Série:", function(ASerie) {
                                inputBox("Digite o Número Inicial:", function(ANumeroInicial) {
                                    inputBox("Digite o Número Final:", function(ANumeroFinal) {
                                        chamaAjaxEnviar({
                                            metodo: "Inutilizar",
                                            ACNPJ: ACNPJ,
                                            AJustificativa: AJustificativa,
                                            AAno: AAno,
                                            AModelo: AModelo,
                                            ASerie: ASerie,
                                            ANumeroInicial: ANumeroInicial,
                                            ANumeroFinal: ANumeroFinal
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });

        $('#EnviarSincronoXml').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoNFe) {
                inputBox("Digite o Lote:", function(ALote) {
                    chamaAjaxEnviar({
                        metodo: "Enviar",
                        tipoArquivo: "xml",
                        AeArquivoNFe: AeArquivoNFe,
                        ALote: ALote,
                        AImprimir: 0,
                        ASincrono: 1,
                        AZipado: 0
                    });
                });
            });
        });

        $('#EnviarAssincronoXml').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoNFe) {
                inputBox("Digite o Lote:", function(ALote) {
                    chamaAjaxEnviar({
                        metodo: "Enviar",
                        tipoArquivo: "xml",
                        AeArquivoNFe: AeArquivoNFe,
                        ALote: ALote,
                        AImprimir: 0,
                        ASincrono: 0,
                        AZipado: 0
                    });
                });
            });
        });

        $('#EnviarSincronoIni').on('click', function() {
            selecionarArquivo(".ini", function(AeArquivoNFe) {
                inputBox("Digite o Lote:", function(ALote) {
                    chamaAjaxEnviar({
                        metodo: "Enviar",
                        tipoArquivo: "ini",
                        AeArquivoNFe: AeArquivoNFe,
                        ALote: ALote,
                        AImprimir: 0,
                        ASincrono: 1,
                        AZipado: 0
                    });
                });
            });
        });

        $('#EnviarAssincronoIni').on('click', function() {
            selecionarArquivo(".ini", function(AeArquivoNFe) {
                inputBox("Digite o Lote:", function(ALote) {
                    chamaAjaxEnviar({
                        metodo: "Enviar",
                        tipoArquivo: "ini",
                        AeArquivoNFe: AeArquivoNFe,
                        ALote: ALote,
                        AImprimir: 0,
                        ASincrono: 0,
                        AZipado: 0
                    });
                });
            });
        });

        $('#ConsultarRecibo').on('click', function() {
            inputBox("Digite o número do recibo:", function(ARecibo) {
                chamaAjaxEnviar({
                    metodo: "ConsultarRecibo",
                    ARecibo: ARecibo
                });
            });
        });

        $('#Cancelar').on('click', function() {
            inputBox("Digite a chave:", function(AeChave) {
                inputBox("Digite a justificativa:", function(AeJustificativa) {
                    inputBox("Digite o CNPJ:", function(AeCNPJCPF) {
                        inputBox("Digite o número do Lote:", function(ALote) {
                            chamaAjaxEnviar({
                                metodo: "Cancelar",
                                AeChave: AeChave,
                                AeJustificativa: AeJustificativa,
                                AeCNPJCPF: AeCNPJCPF,
                                ALote: ALote
                            });
                        });
                    });
                });
            });
        });

        $('#EnviarEvento').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXmlNFe) {
                selecionarArquivo(".xml", function(AeArquivoXmlEvento) {
                    inputBox("Digite o Lote:", function(AidLote) {
                        chamaAjaxEnviar({
                            metodo: "EnviarEvento",
                            AeArquivoXmlNFe: AeArquivoXmlNFe,
                            AeArquivoXmlEvento: AeArquivoXmlEvento,
                            AidLote: AidLote
                        });
                    });
                });
            });
        });

        $('#ConsultaCadastro').on('click', function() {
            inputBox("Digite o código da UF:", function(AcUF) {
                inputBox("Digite o número do documento:", function(AnDocumento) {
                    inputBox("Digite a IE:", function(AnIE) {
                        chamaAjaxEnviar({
                            metodo: "ConsultaCadastro",
                            AcUF: AcUF,
                            AnDocumento: AnDocumento,
                            AnIE: AnIE
                        });
                    }, false);
                }, false);
            });
        });

        $('#DistribuicaoDFePorUltNSU').on('click', function() {
            inputBox("Digite o código da UF:", function(AcUFAutor) {
                inputBox("Digite o CNPJ:", function(AeCNPJCPF) {
                    inputBox("Digite o ultNSU:", function(AeultNSU) {
                        chamaAjaxEnviar({
                            metodo: "DistribuicaoDFePorUltNSU",
                            AcUFAutor: AcUFAutor,
                            AeCNPJCPF: AeCNPJCPF,
                            AeultNSU: AeultNSU
                        });
                    });
                });
            });
        });

        $('#DistribuicaoDFePorNSU').on('click', function() {
            inputBox("Digite o código da UF:", function(AcUFAutor) {
                inputBox("Digite o CNPJ:", function(AeCNPJCPF) {
                    inputBox("Digite o NSU:", function(AeNSU) {
                        chamaAjaxEnviar({
                            metodo: "DistribuicaoDFePorNSU",
                            AcUFAutor: AcUFAutor,
                            AeCNPJCPF: AeCNPJCPF,
                            AeNSU: AeNSU
                        });
                    });
                });
            });
        });

        $('#DistribuicaoDFePorChave').on('click', function() {
            inputBox("Digite o código da UF:", function(AcUFAutor) {
                inputBox("Digite o CNPJ:", function(AeCNPJCPF) {
                    inputBox("Digite a chave:", function(AechNFe) {
                        chamaAjaxEnviar({
                            metodo: "DistribuicaoDFePorChave",
                            AcUFAutor: AcUFAutor,
                            AeCNPJCPF: AeCNPJCPF,
                            AechNFe: AechNFe
                        });
                    });
                });
            });
        });

        $('#EnviarEmail').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXmlNFe) {
                inputBox("Digite o endereço do destinatário:", function(AePara) {
                    inputBox("Digite a chave da NFe:", function(AeChaveNFe) {
                        inputBox("Enviar PDF? (0)Não (1) Sim", function(AEnviaPDF) {
                            chamaAjaxEnviar({
                                metodo: "EnviarEmail",
                                AeArquivoXmlNFe: AeArquivoXmlNFe,
                                AePara: AePara,
                                AeChaveNFe: AeChaveNFe,
                                AEnviaPDF: AEnviaPDF,
                                AeAssunto: $('#emailAssunto').val(),
                                AeCC: "",
                                AeAnexos: "",
                                AeMensagem: $('#emailMensagem').val()
                            });
                        });
                    });
                });
            });
        });

        $('#EnviarEmailEvento').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXmlNFe) {
                selecionarArquivo(".xml", function(AeArquivoXmlEvento) {
                    inputBox("Digite o endereço do destinatário:", function(AePara) {
                        inputBox("Digite a chave do evento:", function(AeChaveEvento) {
                            inputBox("Digite a chave da NFe:", function(AeChaveNFe) {
                                inputBox("Enviar PDF? (0)Não (1) Sim", function(AEnviaPDF) {
                                    chamaAjaxEnviar({
                                        metodo: "EnviarEmailEvento",
                                        AeArquivoXmlNFe: AeArquivoXmlNFe,
                                        AeArquivoXmlEvento: AeArquivoXmlEvento,
                                        AePara: AePara,
                                        AeChaveEvento: AeChaveEvento,
                                        AeChaveNFe: AeChaveNFe,
                                        AEnviaPDF: AEnviaPDF,
                                        AeAssunto: $('#emailAssunto').val(),
                                        AeCC: "",
                                        AeAnexos: "",
                                        AeMensagem: $('#emailMensagem').val()
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });

        $('#ImprimirPDF').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXmlNFe) {
                chamaAjaxEnviar({
                    metodo: "ImprimirPDF",
                    AeArquivoXmlNFe: AeArquivoXmlNFe
                });
            });
        });

        $('#SalvarPDF').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXmlNFe) {
                chamaAjaxEnviar({
                    metodo: "SalvarPDF",
                    AeArquivoXmlNFe: AeArquivoXmlNFe
                });
            });
        });

        $('#ImprimirEventoPDF').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXmlNFe) {
                selecionarArquivo(".xml", function(AeArquivoXmlEvento) {
                    chamaAjaxEnviar({
                        metodo: "ImprimirEventoPDF",
                        AeArquivoXmlNFe: AeArquivoXmlNFe,
                        AeArquivoXmlEvento: AeArquivoXmlEvento
                    });
                });
            });
        });

        $('#SalvarEventoPDF').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXmlNFe) {
                selecionarArquivo(".xml", function(AeArquivoXmlEvento) {
                    chamaAjaxEnviar({
                        metodo: "SalvarEventoPDF",
                        AeArquivoXmlNFe: AeArquivoXmlNFe,
                        AeArquivoXmlEvento: AeArquivoXmlEvento
                    });
                });
            });
        });

        $('#ImprimirInutilizacaoPDF').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXml) {
                chamaAjaxEnviar({
                    metodo: "ImprimirInutilizacaoPDF",
                    AeArquivoXml: AeArquivoXml
                });
            });
        });

        $('#SalvarInutilizacaoPDF').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXml) {
                chamaAjaxEnviar({
                    metodo: "SalvarInutilizacaoPDF",
                    AeArquivoXml: AeArquivoXml
                });
            });
        });

        function chamaAjaxEnviar(infoData) {
            // Fazer a chamada passando o modo ST ou MT. 
            // Ex: http://localhost/NFe/ACBrNFeBase.php?modo=MT
            // Ex: http://localhost/NFe/ACBrNFeBase.php?modo=ST
            var modo = "<?php echo $modo; ?>";
            if (modo == "")
                modo = "MT";

            $.ajax({
                url: modo + '/ACBrNFeServicos' + modo + '.php',
                type: 'POST',
                data: infoData,
                success: function(response) {
                    if ((infoData.metodo === "carregarConfiguracoes") ||
                        (infoData.metodo === "Inicializar")) {
                        processaRetornoConfiguracoes(response);
                    } else {
                        processaResponseGeral(response);
                    }
                },
                error: function(error) {
                    processaResponseGeral(error);
                }
            });
        }

        function inputBox(mensagem, callback, obrigatorio = true) {
            var ACodigoUF = prompt(mensagem);
            if ((obrigatorio) && (ACodigoUF === null || ACodigoUF === "")) {
                alert("Nenhuma informação foi inserida.");
                return;
            }
            callback(ACodigoUF);
        }

        function selecionarArquivo(extensao, retorno) {
            var arquivoSelecionado = document.createElement("input");
            arquivoSelecionado.type = "file";
            arquivoSelecionado.accept = extensao;

            arquivoSelecionado.onchange = function(event) {
                var file = event.target.files[0];

                if (!file) {
                    return;
                }

                var reader = new FileReader();
                reader.onload = function(e) {
                    retorno(e.target.result);
                };
                reader.readAsText(file);
            };

            arquivoSelecionado.click();
        }

        function processaResponseGeral(retorno) {
            if (retorno.mensagem)
                $('#result').val(retorno.mensagem)
            else
                $('#result').val('Erro: ' + JSON.stringify(retorno, null, 4));
        }

        function processaRetornoConfiguracoes(response) {
            if (response.dados) {
                $('#result').val(JSON.stringify(response, null, 4));
                $('#atualizarXml').prop('checked', response.dados.atualizarXml == 1);
                $('#exibirErroSchema').prop('checked', response.dados.exibirErroSchema == 1);
                $('#formatoAlerta').val(response.dados.formatoAlerta);
                $('#formaEmissao').val(response.dados.formaEmissao);
                $('#modeloDF').val(response.dados.modeloDF);
                $('#versaoDF').val(response.dados.versaoDF);
                $('#retirarAcentos').prop('checked', response.dados.retirarAcentos == 1);
                $('#SalvarGer').prop('checked', response.dados.SalvarGer == 1);
                $('#pathSalvar').val(response.dados.pathSalvar);
                $('#pathSchemas').val(response.dados.pathSchemas);
                $('#idCSRT').val(response.dados.idCSRT);
                $('#CSRT').val(response.dados.CSRT);
                $('#SSLType').val(response.dados.SSLType);
                $('#timeout').val(response.dados.timeout);
                $('input[name="ambiente"][value="' + response.dados.ambiente + '"]').prop('checked', true);
                $('#visualizar').prop('checked', response.dados.visualizar == 1);
                $('#SalvarWS').prop('checked', response.dados.SalvarWS == 1);
                $('#ajustaAguardaConsultaRet').prop('checked', response.dados.ajustaAguardaConsultaRet == 1);
                $('#aguardarConsultaRet').val(response.dados.aguardarConsultaRet);
                $('#tentativas').val(response.dados.tentativas);
                $('#intervaloTentativas').val(response.dados.intervaloTentativas);
                $('#SalvarArq').prop('checked', response.dados.SalvarArq == 1);
                $('#SepararPorMes').prop('checked', response.dados.SepararPorMes == 1);
                $('#AdicionarLiteral').prop('checked', response.dados.AdicionarLiteral == 1);
                $('#EmissaoPathNFe').prop('checked', response.dados.EmissaoPathNFe == 1);
                $('#SalvarEvento').prop('checked', response.dados.SalvarEvento == 1);
                $('#SepararPorCNPJ').prop('checked', response.dados.SepararPorCNPJ == 1);
                $('#SepararPorModelo').prop('checked', response.dados.SepararPorModelo == 1);
                $('#PathNFe').val(response.dados.PathNFe);
                $('#PathInu').val(response.dados.PathInu);
                $('#PathEvento').val(response.dados.PathEvento);

                $('#proxyServidor').val(response.dados.proxyServidor);
                $('#proxyPorta').val(response.dados.proxyPorta);
                $('#proxyUsuario').val(response.dados.proxyUsuario);
                $('#proxySenha').val(response.dados.proxySenha);

                $('#UF').val(response.dados.UF);
                $('#SSLCryptLib').val(response.dados.SSLCryptLib);
                $('#SSLHttpLib').val(response.dados.SSLHttpLib);
                $('#SSLXmlSignLib').val(response.dados.SSLXmlSignLib);
                $('#ArquivoPFX').val(response.dados.ArquivoPFX);
                $('#DadosPFX').val(response.dados.DadosPFX);
                $('#senhaCertificado').val(response.dados.senhaCertificado);
                $('#NumeroSerie').val(response.dados.NumeroSerie);

                $('#PathLogo').val(response.dados.PathLogo);
                $('input[name="TipoDANFE"][value="' + response.dados.TipoDANFE + '"]').prop('checked', true);

                $('input[name="TipoRelatorioBobina"][value="' + response.dados.TipoRelatorioBobina + '"]').prop('checked', true);

                $('#PosPrinterModelo').val(response.dados.PosPrinterModelo);
                $('#PaginaDeCodigo').val(response.dados.PaginaDeCodigo);
                $('#PosPrinterPorta').val(response.dados.PosPrinterPorta);
                $('#ColunasFonteNormal').val(response.dados.ColunasFonteNormal);
                $('#EspacoEntreLinhas').val(response.dados.EspacoEntreLinhas);
                $('#LinhasBuffer').val(response.dados.LinhasBuffer);
                $('#LinhasEntreCupons').val(response.dados.LinhasEntreCupons);
                $('#ControlePorta').prop('checked', response.dados.ControlePorta == 1);
                $('#TraduzirTags').prop('checked', response.dados.TraduzirTags == 1);
                $('#CortaPapel').prop('checked', response.dados.CortaPapel == 1);
                $('#IgnorarTags').prop('checked', response.dados.IgnorarTags == 1);

                $('#emailNome').val(response.dados.emailNome);
                $('#emailConta').val(response.dados.emailConta);
                $('#emailServidor').val(response.dados.emailServidor);
                $('#emailPorta').val(response.dados.emailPorta);
                $('#emailSSL').prop('checked', response.dados.emailSSL == 1);
                $('#emailTLS').prop('checked', response.dados.emailTLS == 1);
                $('#emailUsuario').val(response.dados.emailUsuario);
                $('#emailSenha').val(response.dados.emailSenha);
            } else {
                processaResponseGeral(response);
            }
        }
    </script>
</body>

</html>