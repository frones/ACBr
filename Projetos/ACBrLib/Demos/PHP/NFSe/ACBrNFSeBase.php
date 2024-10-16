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
// { ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
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
    <meta name="ACBrNFSe" content="width=device-width, initial-scale=1.0">
    <title>
        <?php
        $title = 'ACBrNFSe';
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
            height: calc(100%);
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

        .panelDireita-container {
            display: none;
        }

        .panelDireita-container.selecionado {
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

        .panelDemaisProvedores-container {
            display: none;
        }

        .panelDemaisProvedores-container.selecionado {
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
                        <button onclick="ativaAba(event, 'emitente', 'panelConfiguracoes')">Emitente</button>
                        <button onclick="ativaAba(event, 'arquivos', 'panelConfiguracoes')">Arquivos</button>
                    </div>
                    <div id="geral" class="panelAba selecionado panelConfiguracoes-container">
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
                        <div class="grid2Col">
                            <label for="MontarPathSchema">Montar automaticamente o Path dos Schemas</label>
                            <input type="checkbox" id="MontarPathSchema">
                        </div>
                        <br>
                        <div class="form-group">
                            <label for="pathSchemas">Pasta dos Schemas do Provedor</label>
                            <input type="text" id="pathSchemas">
                        </div>
                        <div class="form-group">
                            <label for="IniServicos">Arquivo .ini de Serviços</label>
                            <input type="text" id="IniServicos">
                        </div>
                        <div class="grid2Col">
                            <label for="ConsultaLoteAposEnvio">Consultar Lote Após Envio</label>
                            <input type="checkbox" id="ConsultaLoteAposEnvio">
                        </div>
                        <div class="grid2Col">
                            <label for="ConsultaAposCancelar">Consultar Após Cancelar</label>
                            <input type="checkbox" id="ConsultaAposCancelar">
                        </div>
                        <br>
                        <div class="form-group">
                            <label for="LayoutNFSe">LayoutNFSe</label>
                            <select id="LayoutNFSe">
                                <option value="0" selected>lnfsProvedor</option>
                                <option value="1">lnfsPadraoNacionalv1</option>
                            </select>
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
                        <div class="grid2Col">
                            <div class="form-group">
                                <label for="WSUser">Usuário</label>
                                <input type="text" id="WSUser">
                            </div>
                            <div class="form-group">
                                <label for="WSSenha">Senha</label>
                                <input type="text" id="WSSenha">
                            </div>
                        </div>
                        <div class="form-group">
                            <label for="WSFraseSecr">Frase Secreta</label>
                            <input type="text" id="WSFraseSecr">
                        </div>
                        <div class="grid2Col">
                            <div class="form-group">
                                <label for="WSChaveAcesso">Chave de Acesso</label>
                                <input type="text" id="WSChaveAcesso">
                            </div>
                            <div class="form-group">
                                <label for="WSChaveAutoriz">Chave de Autorização</label>
                                <input type="text" id="WSChaveAutoriz">
                            </div>
                        </div>
                        <div class="form-group">
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
                    <div id="emitente" class="panelAba panelConfiguracoes-container">
                        <div class="grid2Col">
                            <div class="form-group">
                                <label for="EmitenteCNPJ">CNPJ</label>
                                <input type="text" id="EmitenteCNPJ">
                            </div>
                            <div class="form-group">
                                <label for="EmitenteInscMun">Inscrição Municipal</label>
                                <input type="text" id="EmitenteInscMun">
                            </div>
                        </div>
                        <div class="form-group">
                            <label for="EmitenteRazSocial">Razão Social</label>
                            <input type="text" id="EmitenteRazSocial">
                        </div>
                        <div class="form-group">
                            <label for="EmitenteNomeFantasia">Fantasia</label>
                            <input type="text" id="EmitenteNomeFantasia">
                        </div>
                        <div class="grid2Col">
                            <div class="form-group">
                                <label for="EmitenteTelefone">Fone</label>
                                <input type="text" id="EmitenteTelefone">
                            </div>
                            <div class="form-group">
                                <label for="EmitenteCEP">CEP</label>
                                <input type="text" id="EmitenteCEP">
                            </div>
                        </div>
                        <div class="form-group">
                            <label for="EmitenteEndereco">Logradouro</label>
                            <input type="text" id="EmitenteEndereco">
                        </div>
                        <div class="grid3Col">
                            <div class="form-group">
                                <label for="EmitenteNumero">Número</label>
                                <input type="text" id="EmitenteNumero">
                            </div>
                            <div class="form-group">
                                <label for="EmitenteComplemento">Complemento</label>
                                <input type="text" id="EmitenteComplemento">
                            </div>
                            <div class="form-group">
                                <label for="EmitenteBairro">Bairro</label>
                                <input type="text" id="EmitenteBairro">
                            </div>
                        </div>
                        <div class="form-group">
                            <label for="CodigoMunicipio">Cidade</label>
                            <select id="CodigoMunicipio">
                                <?php
                                $arquivoMunicipios = 'ACBrMunicipios.txt';

                                if (file_exists($arquivoMunicipios)) {
                                    $linhas = file($arquivoMunicipios, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

                                    foreach ($linhas as $linha) {
                                        if (substr($linha, 0, 2) !== "//") {
                                            list($codigoIBGE, $nomeMunicipio, $uf) = explode('|', $linha);
                                            echo "<option value=$codigoIBGE>$nomeMunicipio/$uf - $codigoIBGE</option>";
                                        }
                                    }
                                } else {
                                    echo "<option value=0>Arquivo de municípios não encontrado</option>";
                                }
                                ?>
                            </select>
                        </div>
                        <div class="grid2Col">
                            <div class="form-group">
                                <label for="CNPJPrefeitura">CNPJ Prefeitura</label>
                                <input type="text" id="CNPJPrefeitura">
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
                            <label for="EmissaoPathNFSe">Salvar Documento pelo campo Dt. Emissão</label>
                            <input type="checkbox" id="EmissaoPathNFSe">
                        </div>
                        <div class="grid2Col">
                            <label for="SepararPorCNPJ">Separar Arqs pelo CNPJ do Certificado</label>
                            <input type="checkbox" id="SepararPorCNPJ">
                        </div>
                        <br>
                        <div class="form-group">
                            <label for="LogPath">Path Log</label>
                            <input type="text" id="LogPath">
                        </div>
                        <div class="grid3Col">
                            <div class="form-group">
                                <label for="LogNivel">Log Nível</label>
                                <select id="LogNivel">
                                    <option value="0">logNenhum</option>
                                    <option value="1">logSimples</option>
                                    <option value="2" selected>logNormal</option>
                                    <option value="3">logCompleto</option>
                                    <option value="4">logParanoico</option>
                                </select>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div id="documento-auxiliar" class="panelAba panelEsquerda-container">
                <div class="form-group">
                    <label for="PathLogo">Logomarca da Prefeitura</label>
                    <input type="text" id="PathLogo">
                </div>
                <div class="form-group">
                    <label for="PrestadorLogo">Logomarca do Prestador de Serviço</label>
                    <input type="text" id="PrestadorLogo">
                </div>
                <div class="form-group">
                    <label for="Prefeitura">Nome da Prefeitura</label>
                    <input type="text" id="Prefeitura">
                </div>
                <div class="form-group">
                    <label for="PathPDF">Pasta PDF</label>
                    <input type="text" id="PathPDF">
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
                <button class="selecionado" onclick="ativaAba(event, 'demaisProvedores', 'panelDireita')">Demais Provedores</button>
                <button onclick="ativaAba(event, 'provedorPadraoNacional', 'panelDireita')">Provedor Padrão Nacional</button>
            </div>
            <div id="demaisProvedores" class="panelAba selecionado panelDireita-container">
                <div class="tabsPanel">
                    <div class="tabAbas">
                        <button class="selecionado" onclick="ativaAba(event, 'envio', 'panelDemaisProvedores')">Envio</button>
                        <button onclick="ativaAba(event, 'consultas', 'panelDemaisProvedores')">Consultas</button>
                        <button onclick="ativaAba(event, 'ConsServPrestados', 'panelDemaisProvedores')">Cons. Serv. Prestados</button>
                        <button onclick="ativaAba(event, 'ConsServTomados', 'panelDemaisProvedores')">Cons. Serv. Tomados</button>
                        <button onclick="ativaAba(event, 'Cancelamento', 'panelDemaisProvedores')">Cancelamento</button>
                    </div>
                    <div id="envio" class="panelAba selecionado panelDemaisProvedores-container">
                        <div class="grid3Col">
                            <input type="button" id="Emitir" value="Emitir Nota">
                            <input type="button" id="EnviarLoteRpsAssincrono" value="Enviar Lote RPS (Assíncrono)">
                            <input type="button" id="EnviarLoteRpsSincrono" value="Enviar Lote RPS (Síncrono)">
                            <input type="button" id="EnviarUmRps" value="Enviar um RPS">
                            <input type="button" id="SubstituirNFSe" value="Substituir NFSe">
                            <input type="button" id="EnviarEmail" value="Enviar Email">
                            <input type="button" id="LinkNFSe" value="Link NFSe">
                            <input type="button" id="GerarToken" value="Gerar Token">
                            <input type="button" id="SalvarPDF" value="Salvar DANFSe (Stream)">
                            <input type="button" id="LimparRepostas" value="Limpar Respostas">
                        </div>
                    </div>
                    <div id="consultas" class="panelAba panelDemaisProvedores-container">
                        <div class="grid3Col">
                            <input type="button" id="ConsultarSituacao" value="Consultar Situação do Lote">
                            <input type="button" id="ConsultarNFSePorPeriodo" value="Consultar NFSe por Período">
                            <input type="button" id="ConsultarNFSePorNumero" value="Consultar NFSe por Numero">
                            <input type="button" id="ConsultarNFSePorRps" value="Consultar NFSe por RPS">
                            <input type="button" id="ConsultarNFSeGenerico" value="Consultar NFSe Genérico">
                            <input type="button" id="ConsultarNFSePorFaixa" value="Consultar NFSe por Faixa">
                            <input type="button" id="ConsultarLoteRps" value="Consultar Lote RPS">
                            <input type="button" id="ConsultarLinkNFSe" value="Consultar Link NFSe">
                        </div>
                    </div>
                    <div id="ConsServPrestados" class="panelAba panelDemaisProvedores-container">
                        <div class="grid2Col">
                            <input type="button" id="ConsultarNFSeServicoPrestadoPorNumero" value="Consultar NFSe Serviço Prestado Por Número">
                            <input type="button" id="ConsultarNFSeServicoPrestadoPorTomador" value="Consultar NFSe Serviço Prestado Por Tomador">
                            <input type="button" id="ConsultarNFSeServicoPrestadoPorPeriodo" value="Consultar NFSe Serviço Prestado Por Periodo">
                            <input type="button" id="ConsultarNFSeServicoPrestadoPorIntermediario" value="Consultar NFSe Serviço Prestado Por Intermediário">
                        </div>
                    </div>
                    <div id="ConsServTomados" class="panelAba panelDemaisProvedores-container">
                        <div class="grid2Col">
                            <input type="button" id="ConsultarNFSeServicoTomadoPorNumero" value="Consultar NFSe Serviço Tomado Por Número">
                            <input type="button" id="ConsultarNFSeServicoTomadoPorPrestador" value="Consultar NFSe Serviço Tomado Por Prestador">
                            <input type="button" id="ConsultarNFSeServicoTomadoPorTomador" value="Consultar NFSe Serviço Tomado Por Tomador">
                            <input type="button" id="ConsultarNFSeServicoTomadoPorPeriodo" value="Consultar NFSe Serviço Tomado Por Periodo">
                            <input type="button" id="ConsultarNFSeServicoTomadoPorIntermediario" value="Consultar NFSe Serviço Tomado Por Intermediário">
                        </div>
                    </div>
                    <div id="Cancelamento" class="panelAba panelDemaisProvedores-container">
                        <div class="grid2Col">
                            <input type="button" id="Cancelar" value="Cancelar NFSe">
                        </div>
                    </div>
                </div>
            </div>
            <div id="provedorPadraoNacional" class="panelAba panelDireita-container">
                <div class="grid3Col">
                    <input type="button" id="EnviarEvento" value="Enviar Evento">
                    <input type="button" id="ConsultarDPSPorChave" value="Consultar DPS Por Chave">
                    <input type="button" id="ConsultarNFSePorChave" value="Consultar NFSe Por Chave">
                    <input type="button" id="ConsultarEvento" value="Consultar Evento">
                    <input type="button" id="ConsultarDFe" value="Consultar DFe">
                    <input type="button" id="ObterDANFSE" value="Obter DANFSe">
                    <input type="button" id="ConsultarParametros" value="Consultar Parâmetros">
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
                const abaSelecionada = document.querySelector(`#${abaId}.panelDireita-container`);
                const outrasAbas = document.querySelectorAll('.panelDireita-container');
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

            // Controle panel Demais Provedores
            if (panel === 'panelDemaisProvedores') {
                const abaSelecionada = document.querySelector(`#${abaId}.panelDemaisProvedores-container`);
                const outrasAbas = document.querySelectorAll('.panelDemaisProvedores-container');
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
                LogPath: $('#LogPath').val(),
                LogNivel: $('#LogNivel').val(),

                metodo: "salvarConfiguracoes",
                exibirErroSchema: $('#exibirErroSchema').prop('checked') ? 1 : 0,
                formatoAlerta: $('#formatoAlerta').val(),
                formaEmissao: $('#formaEmissao').val(),
                retirarAcentos: $('#retirarAcentos').prop('checked') ? 1 : 0,
                SalvarGer: $('#SalvarGer').prop('checked') ? 1 : 0,
                pathSalvar: $('#pathSalvar').val(),
                MontarPathSchema: $('#MontarPathSchema').prop('checked') ? 1 : 0,
                pathSchemas: $('#pathSchemas').val(),
                IniServicos: $('#IniServicos').val(),
                ConsultaLoteAposEnvio: $('#ConsultaLoteAposEnvio').prop('checked') ? 1 : 0,
                ConsultaAposCancelar: $('#ConsultaAposCancelar').prop('checked') ? 1 : 0,
                LayoutNFSe: $('#LayoutNFSe').val(),
                SSLType: $('#SSLType').val(),
                timeout: $('#timeout').val(),
                ambiente: parseInt($('input[name="ambiente"]:checked').val(), 10),
                visualizar: $('#visualizar').prop('checked') ? 1 : 0,
                SalvarWS: $('#SalvarWS').prop('checked') ? 1 : 0,
                WSUser: $('#WSUser').val(),
                WSSenha: $('#WSSenha').val(),
                WSFraseSecr: $('#WSFraseSecr').val(),
                WSChaveAcesso: $('#WSChaveAcesso').val(),
                WSChaveAutoriz: $('#WSChaveAutoriz').val(),
                EmitenteCNPJ: $('#EmitenteCNPJ').val(),
                EmitenteInscMun: $('#EmitenteInscMun').val(),
                EmitenteRazSocial: $('#EmitenteRazSocial').val(),
                EmitenteNomeFantasia: $('#EmitenteNomeFantasia').val(),
                EmitenteTelefone: $('#EmitenteTelefone').val(),
                EmitenteCEP: $('#EmitenteCEP').val(),
                EmitenteEndereco: $('#EmitenteEndereco').val(),
                EmitenteNumero: $('#EmitenteNumero').val(),
                EmitenteComplemento: $('#EmitenteComplemento').val(),
                EmitenteBairro: $('#EmitenteBairro').val(),
                CodigoMunicipio: $('#CodigoMunicipio').val(),
                CNPJPrefeitura: $('#CNPJPrefeitura').val(),

                ajustaAguardaConsultaRet: $('#ajustaAguardaConsultaRet').prop('checked') ? 1 : 0,
                aguardarConsultaRet: $('#aguardarConsultaRet').val(),
                tentativas: $('#tentativas').val(),
                intervaloTentativas: $('#intervaloTentativas').val(),
                SalvarArq: $('#SalvarArq').prop('checked') ? 1 : 0,
                SepararPorMes: $('#SepararPorMes').prop('checked') ? 1 : 0,
                AdicionarLiteral: $('#AdicionarLiteral').prop('checked') ? 1 : 0,
                EmissaoPathNFSe: $('#EmissaoPathNFSe').prop('checked') ? 1 : 0,
                SepararPorCNPJ: $('#SepararPorCNPJ').prop('checked') ? 1 : 0,

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

                PathLogo: $('#PathLogo').val(),
                PrestadorLogo: $('#PrestadorLogo').val(),
                Prefeitura: $('#Prefeitura').val(),
                PathPDF: $('#PathPDF').val(),

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

        $('#LimparRepostas').on('click', function() {
            $.ajax(
                $('#result').val(""));
        });

        $('#OpenSSLInfo').on('click', function() {
            chamaAjaxEnviar({
                metodo: "OpenSSLInfo"
            });
        });

        function Emitir(AModoEnvio) {
            selecionarArquivo(".xml;*.ini", function(AeArquivoXmlOuIni) {
                inputBox("Digite o Lote:", function(ALote) {
                    chamaAjaxEnviar({
                        metodo: "Emitir",
                        AModoEnvio: AModoEnvio,
                        ALote: ALote,
                        AeArquivoXmlOuIni: AeArquivoXmlOuIni
                    });
                });
            });
        };

        $('#Emitir').on('click', function() {
            Emitir(0);
        });

        $('#EnviarLoteRpsAssincrono').on('click', function() {
            Emitir(1);
        });

        $('#EnviarLoteRpsSincrono').on('click', function() {
            Emitir(2);
        });

        $('#EnviarUmRps').on('click', function() {
            Emitir(3);
        });

        $('#ConsultarSituacao').on('click', function() {
            inputBox("Digite o Protocolo:", function(AProtocolo) {
                inputBox("Digite o Número do Lote:", function(ANumLote) {
                    chamaAjaxEnviar({
                        metodo: "ConsultarSituacao",
                        AProtocolo: AProtocolo,
                        ANumLote: ANumLote
                    });
                });
            });
        });

        $('#ConsultarLoteRps').on('click', function() {
            inputBox("Digite o Protocolo:", function(AProtocolo) {
                inputBox("Digite o Número do Lote:", function(ANumLote) {
                    chamaAjaxEnviar({
                        metodo: "ConsultarLoteRps",
                        AProtocolo: AProtocolo,
                        ANumLote: ANumLote
                    });
                });
            });
        });

        $('#ConsultarNFSePorPeriodo').on('click', function() {
            inputBox("Informe a Data Inicial (dd/mm/aaaa):", function(ADataInicial) {
                inputBox("Informe a Data Final (dd/mm/aaaa):", function(ADataFinal) {
                    inputBox("Digite o Número da Página:", function(APagina) {
                        inputBox("Digite o Número do Lote:", function(ANumeroLote) {
                            inputBox("Digite o Tipo do Período (0)Emissão (1)Competência:", function(ATipoPeriodo) {
                                chamaAjaxEnviar({
                                    metodo: "ConsultarNFSePorPeriodo",
                                    ADataInicial: ADataInicial,
                                    ADataFinal: ADataFinal,
                                    APagina: APagina,
                                    ANumeroLote: ANumeroLote,
                                    ATipoPeriodo: ATipoPeriodo
                                });
                            }, false);
                        }, false);
                    });
                });
            });
        });

        $('#ConsultarNFSePorNumero').on('click', function() {
            inputBox("Digite o Número da NFSe:", function(ANumero) {
                inputBox("Digite o Número da Página:", function(APagina) {
                    chamaAjaxEnviar({
                        metodo: "ConsultarNFSePorNumero",
                        ANumero: ANumero,
                        APagina: APagina
                    });
                });
            });
        });

        $('#ConsultarNFSePorRps').on('click', function() {
            inputBox("Informe o Número do RPS:", function(ANumeroRps) {
                inputBox("Informe a Série do RPS:", function(ASerie) {
                    inputBox("Informe o Tipo:", function(ATipo) {
                        inputBox("Informe o Código de Verificação:", function(ACodigoVerificacao) {
                            chamaAjaxEnviar({
                                metodo: "ConsultarNFSePorRps",
                                ANumeroRps: ANumeroRps,
                                ASerie: ASerie,
                                ATipo: ATipo,
                                ACodigoVerificacao: ACodigoVerificacao
                            });
                        }, false);
                    }, false);
                });
            });
        });

        $('#ConsultarNFSeGenerico').on('click', function() {
            selecionarArquivo(".ini", function(AInfConsultaNFSe) {
                chamaAjaxEnviar({
                    metodo: "ConsultarNFSeGenerico",
                    AInfConsultaNFSe: AInfConsultaNFSe
                });
            });
        });

        $('#ConsultarNFSePorFaixa').on('click', function() {
            inputBox("Digite o Número Inicial:", function(ANumeroInicial) {
                inputBox("Digite o Número Final:", function(ANumeroFinal) {
                    inputBox("Digite o Número da Página:", function(APagina) {
                        chamaAjaxEnviar({
                            metodo: "ConsultarNFSePorFaixa",
                            ANumeroInicial: ANumeroInicial,
                            ANumeroFinal: ANumeroFinal,
                            APagina: APagina
                        });
                    });
                });
            });
        });

        $('#ConsultarLinkNFSe').on('click', function() {
            selecionarArquivo(".ini", function(AInfConsultaLinkNFSe) {
                chamaAjaxEnviar({
                    metodo: "ConsultarLinkNFSe",
                    AInfConsultaLinkNFSe: AInfConsultaLinkNFSe
                });
            });
        });

        $('#EnviarEmail').on('click', function() {
            selecionarArquivo(".xml", function(AeXmlNFSe) {
                inputBox("Digite o endereço do destinatário:", function(AePara) {
                    inputBox("Enviar PDF? (0)Não (1) Sim", function(AEnviaPDF) {
                        chamaAjaxEnviar({
                            metodo: "EnviarEmail",
                            AeXmlNFSe: AeXmlNFSe,
                            AePara: AePara,
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

        $('#LinkNFSe').on('click', function() {
            inputBox("Informe o Número da NFSe:", function(ANumeroNFSe) {
                inputBox("Informe o Código de Verificação:", function(ACodigoVerificacao) {
                    inputBox("Informe a Chave de Acesso :", function(AChaveAcesso) {
                        inputBox("Informe o Valor do Serviço:", function(AValorServico) {
                            chamaAjaxEnviar({
                                metodo: "LinkNFSe",
                                ANumeroNFSe: ANumeroNFSe,
                                ACodigoVerificacao: ACodigoVerificacao,
                                AChaveAcesso: AChaveAcesso,
                                AValorServico: AValorServico
                            });
                        });
                    });
                });
            });
        });

        $('#GerarToken').on('click', function() {
            chamaAjaxEnviar({
                metodo: "GerarToken"
            });
        });

        $('#SalvarPDF').on('click', function() {
            selecionarArquivo(".xml", function(AeArquivoXml) {
                chamaAjaxEnviar({
                    metodo: "SalvarPDF",
                    AeArquivoXml: AeArquivoXml
                });
            });
        });

        $('#SubstituirNFSe').on('click', function() {
            selecionarArquivo(".xml;*.ini", function(AeArquivoXmlOuIni) {
                inputBox("Informe o Número da NFSe:", function(ANumeroNFSe) {
                    inputBox("Informe a Série da NFSe:", function(ASerieNFSe) {
                        inputBox("Informe o Código de Cancelamento:", function(ACodigoCancelamento) {
                            inputBox("Informe o Motivo do Cancelamento:", function(AMotivoCancelamento) {
                                inputBox("Informe o Número do Lote:", function(ANumeroLote) {
                                    inputBox("Informe o Código de Verificação:", function(ACodigoVerificacao) {
                                        chamaAjaxEnviar({
                                            metodo: "SubstituirNFSe",
                                            AeArquivoXmlOuIni: AeArquivoXmlOuIni,
                                            ANumeroNFSe: ANumeroNFSe,
                                            ASerieNFSe: ASerieNFSe,
                                            ACodigoCancelamento: ACodigoCancelamento,
                                            AMotivoCancelamento: AMotivoCancelamento,
                                            ANumeroLote: ANumeroLote,
                                            ACodigoVerificacao: ACodigoVerificacao
                                        }, false);
                                    }, false);
                                }, false);
                            }, false);
                        });
                    });
                });
            });
        });

        $('#Cancelar').on('click', function() {
            selecionarArquivo(".ini", function(AInfCancelamentoNFSe) {
                chamaAjaxEnviar({
                    metodo: "Cancelar",
                    AInfCancelamentoNFSe: AInfCancelamentoNFSe
                });
            });
        });

        $('#ConsultarNFSeServicoPrestadoPorNumero').on('click', function() {
            inputBox("Digite o Número da NFSe:", function(ANumero) {
                inputBox("Digite o Número da Página:", function(APagina) {
                    inputBox("Informe a Data Inicial (dd/mm/aaaa):", function(ADataInicial) {
                        inputBox("Informe a Data Final (dd/mm/aaaa):", function(ADataFinal) {
                            inputBox("Digite o Tipo do Período (0)Emissão (1)Competência:", function(ATipoPeriodo) {
                                chamaAjaxEnviar({
                                    metodo: "ConsultarNFSeServicoPrestadoPorNumero",
                                    ANumero: ANumero,
                                    APagina: APagina,
                                    ADataInicial: ADataInicial,
                                    ADataFinal: ADataFinal,
                                    ATipoPeriodo: ATipoPeriodo
                                });
                            });
                        });
                    });
                });
            });
        });

        $('#ConsultarNFSeServicoPrestadoPorTomador').on('click', function() {
            inputBox("Informe o CNPJ:", function(ACNPJ) {
                inputBox("Informe a Inscrição Municipal:", function(AInscMun) {
                    inputBox("Digite o Número da Página:", function(APagina) {
                        inputBox("Informe a Data Inicial (dd/mm/aaaa):", function(ADataInicial) {
                            inputBox("Informe a Data Final (dd/mm/aaaa):", function(ADataFinal) {
                                inputBox("Digite o Tipo do Período (0)Emissão (1)Competência:", function(ATipoPeriodo) {
                                    chamaAjaxEnviar({
                                        metodo: "ConsultarNFSeServicoPrestadoPorTomador",
                                        ACNPJ: ACNPJ,
                                        AInscMun: AInscMun,
                                        APagina: APagina,
                                        ADataInicial: ADataInicial,
                                        ADataFinal: ADataFinal,
                                        ATipoPeriodo: ATipoPeriodo
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });

        $('#ConsultarNFSeServicoPrestadoPorPeriodo').on('click', function() {
            inputBox("Informe a Data Inicial (dd/mm/aaaa):", function(ADataInicial) {
                inputBox("Informe a Data Final (dd/mm/aaaa):", function(ADataFinal) {
                    inputBox("Digite o Número da Página:", function(APagina) {
                        inputBox("Digite o Tipo do Período (0)Emissão (1)Competência:", function(ATipoPeriodo) {
                            chamaAjaxEnviar({
                                metodo: "ConsultarNFSeServicoPrestadoPorPeriodo",
                                ADataInicial: ADataInicial,
                                ADataFinal: ADataFinal,
                                APagina: APagina,
                                ATipoPeriodo: ATipoPeriodo
                            });
                        });
                    });
                });
            });
        });

        $('#ConsultarNFSeServicoPrestadoPorIntermediario').on('click', function() {
            inputBox("Informe o CNPJ:", function(ACNPJ) {
                inputBox("Informe a Inscrição Municipal:", function(AInscMun) {
                    inputBox("Digite o Número da Página:", function(APagina) {
                        inputBox("Informe a Data Inicial (dd/mm/aaaa):", function(ADataInicial) {
                            inputBox("Informe a Data Final (dd/mm/aaaa):", function(ADataFinal) {
                                inputBox("Digite o Tipo do Período (0)Emissão (1)Competência:", function(ATipoPeriodo) {
                                    chamaAjaxEnviar({
                                        metodo: "ConsultarNFSeServicoPrestadoPorIntermediario",
                                        ACNPJ: ACNPJ,
                                        AInscMun: AInscMun,
                                        APagina: APagina,
                                        ADataInicial: ADataInicial,
                                        ADataFinal: ADataFinal,
                                        ATipoPeriodo: ATipoPeriodo
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });

        $('#ConsultarNFSeServicoTomadoPorNumero').on('click', function() {
            inputBox("Digite o Número da NFSe:", function(ANumero) {
                inputBox("Digite o Número da Página:", function(APagina) {
                    inputBox("Informe a Data Inicial (dd/mm/aaaa):", function(ADataInicial) {
                        inputBox("Informe a Data Final (dd/mm/aaaa):", function(ADataFinal) {
                            inputBox("Digite o Tipo do Período (0)Emissão (1)Competência:", function(ATipoPeriodo) {
                                chamaAjaxEnviar({
                                    metodo: "ConsultarNFSeServicoTomadoPorNumero",
                                    ANumero: ANumero,
                                    APagina: APagina,
                                    ADataInicial: ADataInicial,
                                    ADataFinal: ADataFinal,
                                    ATipoPeriodo: ATipoPeriodo
                                });
                            });
                        });
                    });
                });
            });
        });

        $('#ConsultarNFSeServicoTomadoPorTomador').on('click', function() {
            inputBox("Informe o CNPJ:", function(ACNPJ) {
                inputBox("Informe a Inscrição Municipal:", function(AInscMun) {
                    inputBox("Digite o Número da Página:", function(APagina) {
                        inputBox("Informe a Data Inicial (dd/mm/aaaa):", function(ADataInicial) {
                            inputBox("Informe a Data Final (dd/mm/aaaa):", function(ADataFinal) {
                                inputBox("Digite o Tipo do Período (0)Emissão (1)Competência:", function(ATipoPeriodo) {
                                    chamaAjaxEnviar({
                                        metodo: "ConsultarNFSeServicoTomadoPorTomador",
                                        ACNPJ: ACNPJ,
                                        AInscMun: AInscMun,
                                        APagina: APagina,
                                        ADataInicial: ADataInicial,
                                        ADataFinal: ADataFinal,
                                        ATipoPeriodo: ATipoPeriodo
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });

        $('#ConsultarNFSeServicoTomadoPorPeriodo').on('click', function() {
            inputBox("Informe a Data Inicial (dd/mm/aaaa):", function(ADataInicial) {
                inputBox("Informe a Data Final (dd/mm/aaaa):", function(ADataFinal) {
                    inputBox("Digite o Número da Página:", function(APagina) {
                        inputBox("Digite o Tipo do Período (0)Emissão (1)Competência:", function(ATipoPeriodo) {
                            chamaAjaxEnviar({
                                metodo: "ConsultarNFSeServicoTomadoPorPeriodo",
                                ADataInicial: ADataInicial,
                                ADataFinal: ADataFinal,
                                APagina: APagina,
                                ATipoPeriodo: ATipoPeriodo
                            });
                        });
                    });
                });
            });
        });

        $('#ConsultarNFSeServicoTomadoPorIntermediario').on('click', function() {
            inputBox("Informe o CNPJ:", function(ACNPJ) {
                inputBox("Informe a Inscrição Municipal:", function(AInscMun) {
                    inputBox("Digite o Número da Página:", function(APagina) {
                        inputBox("Informe a Data Inicial (dd/mm/aaaa):", function(ADataInicial) {
                            inputBox("Informe a Data Final (dd/mm/aaaa):", function(ADataFinal) {
                                inputBox("Digite o Tipo do Período (0)Emissão (1)Competência:", function(ATipoPeriodo) {
                                    chamaAjaxEnviar({
                                        metodo: "ConsultarNFSeServicoTomadoPorIntermediario",
                                        ACNPJ: ACNPJ,
                                        AInscMun: AInscMun,
                                        APagina: APagina,
                                        ADataInicial: ADataInicial,
                                        ADataFinal: ADataFinal,
                                        ATipoPeriodo: ATipoPeriodo
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });

        $('#EnviarEvento').on('click', function() {
            selecionarArquivo(".ini", function(AInfEvento) {
                chamaAjaxEnviar({
                    metodo: "EnviarEvento",
                    AInfEvento: AInfEvento
                });
            });
        });

        $('#ConsultarDPSPorChave').on('click', function() {
            inputBox("Informe a Chave do DPS:", function(AChaveDPS) {
                chamaAjaxEnviar({
                    metodo: "ConsultarDPSPorChave",
                    AChaveDPS: AChaveDPS
                });
            });
        });

        $('#ConsultarNFSePorChave').on('click', function() {
            inputBox("Informe a Chave da NFSe:", function(AChaveNFSe) {
                chamaAjaxEnviar({
                    metodo: "ConsultarNFSePorChave",
                    AChaveNFSe: AChaveNFSe
                });
            });
        });

        $('#ConsultarEvento').on('click', function() {
            inputBox("Informe a Chave:", function(AChave) {
                inputBox("Informe o Tipo de Evento:", function(ATipoEvento) {
                    inputBox("Informe o Número Sequencia:", function(ANumSeq) {
                        chamaAjaxEnviar({
                            metodo: "ConsultarEvento",
                            AChave: AChave,
                            ATipoEvento: ATipoEvento,
                            ANumSeq: ANumSeq
                        });
                    });
                });
            });
        });

        $('#ConsultarDFe').on('click', function() {
            inputBox("Informe o NSU:", function(ANSU) {
                chamaAjaxEnviar({
                    metodo: "ConsultarDFe",
                    ANSU: ANSU
                });
            });
        });

        $('#ObterDANFSE').on('click', function() {
            inputBox("Informe a Chave da NFSe:", function(AChaveNFSe) {
                chamaAjaxEnviar({
                    metodo: "ObterDANFSE",
                    AChaveNFSe: AChaveNFSe
                });
            });
        });

        $('#ConsultarParametros').on('click', function() {
            inputBox("Informe o Parametro do Municipio:", function(ATipoParametroMunicipio) {
                inputBox("Informe o Código de Serviço:", function(ACodigoServico) {
                    inputBox("Informe a Competencia (dd/mm/aaaa):", function(ACompetencia) {
                        inputBox("Informe o Numero do Beneficio:", function(ANumeroBeneficio) {
                            chamaAjaxEnviar({
                                metodo: "ConsultarParametros",
                                ATipoParametroMunicipio: ATipoParametroMunicipio,
                                ACodigoServico: ACodigoServico,
                                ACompetencia: ACompetencia,
                                ANumeroBeneficio: ANumeroBeneficio
                            });
                        });
                    });
                });
            });
        });

        function chamaAjaxEnviar(infoData) {
            // Fazer a chamada passando o modo ST ou MT. 
            // Ex: http://localhost/NFSe/ACBrNFSeBase.php?modo=MT
            // Ex: http://localhost/NFSe/ACBrNFSeBase.php?modo=ST
            var modo = "<?php echo $modo; ?>";
            if (modo == "")
                modo = "MT";

            $.ajax({
                url: modo + '/ACBrNFSeServicos' + modo + '.php',
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
            var retorno = prompt(mensagem);
            if ((obrigatorio) && (retorno === null || retorno === "")) {
                alert("Nenhuma informação foi inserida.");
                return;
            }
            callback(retorno);
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

                $('#LogPath').val(response.dados.LogPath);
                $('#LogNivel').val(response.dados.LogNivel);

                $('#exibirErroSchema').prop('checked', response.dados.exibirErroSchema == 1);
                $('#formatoAlerta').val(response.dados.formatoAlerta);
                $('#formaEmissao').val(response.dados.formaEmissao);
                $('#retirarAcentos').prop('checked', response.dados.retirarAcentos == 1);
                $('#SalvarGer').prop('checked', response.dados.SalvarGer == 1);
                $('#pathSalvar').val(response.dados.pathSalvar);
                $('#MontarPathSchema').prop('checked', response.dados.MontarPathSchema == 1);
                $('#pathSchemas').val(response.dados.pathSchemas);
                $('#IniServicos').val(response.dados.IniServicos);
                $('#ConsultaLoteAposEnvio').prop('checked', response.dados.ConsultaLoteAposEnvio == 1);
                $('#ConsultaAposCancelar').prop('checked', response.dados.ConsultaAposCancelar == 1);
                $('#LayoutNFSe').val(response.dados.LayoutNFSe);
                $('#SSLType').val(response.dados.SSLType);
                $('#timeout').val(response.dados.timeout);
                $('input[name="ambiente"][value="' + response.dados.ambiente + '"]').prop('checked', true);
                $('#visualizar').prop('checked', response.dados.visualizar == 1);
                $('#SalvarWS').prop('checked', response.dados.SalvarWS == 1);
                $('#WSUser').val(response.dados.WSUser);
                $('#WSSenha').val(response.dados.WSSenha);
                $('#WSFraseSecr').val(response.dados.WSFraseSecr);
                $('#WSChaveAcesso').val(response.dados.WSChaveAcesso);
                $('#WSChaveAutoriz').val(response.dados.WSChaveAutoriz);
                $('#EmitenteCNPJ').val(response.dados.EmitenteCNPJ);
                $('#EmitenteInscMun').val(response.dados.EmitenteInscMun);
                $('#EmitenteRazSocial').val(response.dados.EmitenteRazSocial);
                $('#EmitenteNomeFantasia').val(response.dados.EmitenteNomeFantasia);
                $('#EmitenteTelefone').val(response.dados.EmitenteTelefone);
                $('#EmitenteCEP').val(response.dados.EmitenteCEP);
                $('#EmitenteEndereco').val(response.dados.EmitenteEndereco);
                $('#EmitenteNumero').val(response.dados.EmitenteNumero);
                $('#EmitenteComplemento').val(response.dados.EmitenteComplemento);
                $('#EmitenteBairro').val(response.dados.EmitenteBairro);
                $('#CodigoMunicipio').val(response.dados.CodigoMunicipio);
                $('#CNPJPrefeitura').val(response.dados.CNPJPrefeitura);

                $('#ajustaAguardaConsultaRet').prop('checked', response.dados.ajustaAguardaConsultaRet == 1);
                $('#aguardarConsultaRet').val(response.dados.aguardarConsultaRet);
                $('#tentativas').val(response.dados.tentativas);
                $('#intervaloTentativas').val(response.dados.intervaloTentativas);
                $('#SalvarArq').prop('checked', response.dados.SalvarArq == 1);
                $('#SepararPorMes').prop('checked', response.dados.SepararPorMes == 1);
                $('#AdicionarLiteral').prop('checked', response.dados.AdicionarLiteral == 1);
                $('#EmissaoPathNFSe').prop('checked', response.dados.EmissaoPathNFSe == 1);
                $('#SepararPorCNPJ').prop('checked', response.dados.SepararPorCNPJ == 1);

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
                $('#PrestadorLogo').val(response.dados.PrestadorLogo);
                $('#Prefeitura').val(response.dados.Prefeitura);
                $('#PathPDF').val(response.dados.PathPDF);

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