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
    <meta name="ACBrBoleto" content="width=device-width, initial-scale=1.0">
    <title>
        <?php
        $title = 'ACBrBoleto';
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
                <button class="selecionado" onclick="ativaAba(event, 'cedente', 'panelEsquerda')">Cedente</button>
                <button onclick="ativaAba(event, 'abaLayout', 'panelEsquerda')">Layout</button>
                <button onclick="ativaAba(event, 'contaBancaria', 'panelEsquerda')">Conta Bancária</button>
                <button onclick="ativaAba(event, 'remessaRetorno', 'panelEsquerda')">Remessa/Retorno</button>
                <button onclick="ativaAba(event, 'email', 'panelEsquerda')">Email</button>
                <button onclick="ativaAba(event, 'webservices', 'panelEsquerda')">WebService</button>
            </div>
            <div id="cedente" class="panelAba selecionado panelEsquerda-container">
                <div class="tabsPanel">
                    <div class="grid2Col">
                        <div class="grid2Col">
                            <div class="form-group">
                                <label for="TipoInscricao">Pessoa</label>
                                <select id="TipoInscricao" name="TipoInscricao">
                                    <option value="0" selected>pFisica</option>
                                    <option value="1">pJuridica</option>
                                    <option value="2">pOutras</option>
                                    <option value="3">pNenhum</option>
                                </select>
                            </div>
                            <div class="form-group">
                                <label for="CNPJCPF">CNPJ/CPF</label>
                                <input type="text" id="CNPJCPF">
                            </div>
                        </div>
                        <div class="form-group">
                            <label for="Nome">Nome / Razão Social</label>
                            <input type="text" id="Nome">
                        </div>
                    </div>
                    <div class="grid2Col">
                        <div class="form-group">
                            <label for="Logradouro">Logradouro</label>
                            <input type="text" id="Logradouro">
                        </div>
                        <div class="grid2Col">
                            <div class="form-group">
                                <label for="NumeroRes">Número</label>
                                <input type="text" id="NumeroRes">
                            </div>
                            <div class="form-group">
                                <label for="Bairro">Bairro</label>
                                <input type="text" id="Bairro">
                            </div>
                        </div>
                    </div>
                    <div class="grid3Col">
                        <div class="form-group">
                            <label for="Complemento">Complemento</label>
                            <input type="text" id="Complemento">
                        </div>
                        <div class="form-group">
                            <label for="Cidade">Cidade</label>
                            <input type="text" id="Cidade">
                        </div>
                        <div class="grid2Col">
                            <div class="form-group">
                                <label for="UF">UF</label>
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
                                <label for="CEP">CEP</label>
                                <input type="text" id="CEP">
                            </div>
                        </div>
                    </div>
                    <div class="grid3Col">
                        <div class="form-group">
                            <label for="Telefone">Telefone</label>
                            <input type="text" id="Telefone">
                        </div>
                        <div class="form-group">
                            <label for="TipoDocumento">Documento</label>
                            <select id="TipoDocumento" name="TipoDocumento">
                                <option value="0" selected>Tradicional</option>
                                <option value="1">Escritural</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label for="TipoCarteira">Carteira</label>
                            <select id="TipoCarteira" name="TipoCarteira">
                                <option value="0" selected>tctSimples</option>
                                <option value="1">tctRegistrada</option>
                                <option value="2">tctEletronica</option>
                            </select>
                        </div>
                    </div>
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
            <div id="abaLayout" class="panelAba panelEsquerda-container">
                <div class="grid2Col">
                    <div class="grid2Col">
                        <div class="form-group">
                            <label for="PrinterName">Impressora</label>
                            <input type="text" id="PrinterName">
                        </div>
                        <div class="form-group">
                            <label for="Layout">Layout</label>
                            <select id="Layout" name="Layout">
                                <option value="0" selected>Padrão</option>
                                <option value="1">Carnê</option>
                                <option value="2">Fatura</option>
                                <option value="3">Padrão Entrega</option>
                                <option value="4">Recibo Topo</option>
                                <option value="5">Padrão Entrega 2</option>
                                <option value="6">Fatura Detalhe</option>
                                <option value="7">Térmica 80mm</option>
                                <option value="8">Padrão PIX</option>
                                <option value="9">Presta Serviços</option>
                            </select>
                        </div>
                    </div>
                    <div class="form-group">
                        <label for="DirLogo">Diretório Logotipo</label>
                        <input type="text" id="DirLogo">
                    </div>
                </div>
                <div class="grid2Col">
                    <div class="grid2Col">
                        <div class="form-group">
                            <label for="NumeroCopias">Número Cópias</label>
                            <input type="text" id="NumeroCopias">
                        </div>
                    </div>
                    <div class="form-group">
                        <label for="NomeArquivo">Nome Arquivo PDF</label>
                        <input type="text" id="NomeArquivo">
                    </div>
                </div>
            </div>
            <div id="contaBancaria" class="panelAba panelEsquerda-container">
                <div class="grid2Col">
                    <div class="form-group">
                        <label for="TipoCobranca">Banco</label>
                        <select id="TipoCobranca" name="TipoCobranca">
                            <option value="0">Nenhum</option>
                            <option value="1" selected>Banco do Brasil</option>
                            <option value="2">Santander</option>
                            <option value="3">Caixa Econômica Federal (Convênio SIGCB)</option>
                            <option value="4">Caixa Econômica Federal (Convênio SICOB)</option>
                            <option value="5">Bradesco</option>
                            <option value="6">Itaú</option>
                            <option value="7">Banco Mercantil</option>
                            <option value="8">Sicred</option>
                            <option value="9">Bancoob</option>
                            <option value="10">Banrisul</option>
                            <option value="11">Banestes</option>
                            <option value="12">HSBC</option>
                            <option value="13">Banco do Nordeste</option>
                            <option value="14">Banco BRB </option>
                            <option value="15">BicBanco</option>
                            <option value="16">BradescoSICOOB</option>
                            <option value="17">BancoSafra</option>
                            <option value="18">SafraBradesco</option>
                            <option value="19">BancoCECRED</option>
                            <option value="20">BancoDaAmazonia</option>
                            <option value="21">BancoDoBrasilSICOOB</option>
                            <option value="22">Uniprime</option>
                            <option value="23">UnicredRS</option>
                            <option value="24">Banese</option>
                            <option value="25">CrediSIS</option>
                            <option value="26">UnicredES </option>
                            <option value="27">BancoCresolSCRS</option>
                            <option value="28">CitiBank</option>
                            <option value="29">BancoABCBrasil</option>
                            <option value="30">Daycoval</option>
                            <option value="31">UniprimeNortePR</option>
                            <option value="32">BancoPine</option>
                            <option value="33">BancoPineBradesco</option>
                            <option value="34">UnicredSC</option>
                            <option value="35">BancoAlfa,</option>
                            <option value="36">BancoDoBrasilAPI</option>
                            <option value="37">BancoDoBrasilWS</option>
                            <option value="38">BancoCresol</option>
                            <option value="39">MoneyPlus </option>
                            <option value="40">BancoC6</option>
                            <option value="41">BancoRendimento</option>
                            <option value="42">BancoInter</option>
                            <option value="43">BancoSofisaSantander</option>
                            <option value="44">BS2</option>
                            <option value="45">PenseBankAPI</option>
                            <option value="46">BTGPactual</option>
                            <option value="47">BancoOriginal</option>
                            <option value="48">BancoVotorantin</option>
                            <option value="49">BancoPefisa</option>
                            <option value="50">BancoFibra</option>
                            <option value="51">BancoSofisaItau</option>
                            <option value="52">BancoIndustrialBrasil</option>
                            <option value="53">BancoAthenaBradesco</option>
                            <option value="54">BancoQITechSCD</option>
                            <option value="55">BancoUY3</option>
                            <option value="56">BancoBocomBBM</option>
                            <option value="57">BancoSicoob</option>
                            <option value="58">BancoSisprime</option>
                            <option value="59">BancoAilos"</option>
                        </select>
                    </div>
                    <div class="form-group">
                        <label for="ResponEmissao">Resp. Emissão</label>
                        <select id="ResponEmissao" name="ResponEmissao">
                            <option value="0" selected>Cliente Emite</option>
                            <option value="1">Banco Emite</option>
                            <option value="2">Banco Reemite</option>
                            <option value="3">Banco não Reemite</option>
                        </select>
                    </div>
                </div>
                <div class="grid4Col">
                    <div class="form-group">
                        <label for="Agencia">Agência</label>
                        <input type="text" id="Agencia">
                    </div>
                    <div class="form-group">
                        <label for="AgenciaDigito">DV Agência</label>
                        <input type="text" id="AgenciaDigito">
                    </div>
                    <div class="form-group">
                        <label for="Conta">Conta</label>
                        <input type="text" id="Conta">
                    </div>
                    <div class="form-group">
                        <label for="ContaDigito">DV Conta</label>
                        <input type="text" id="ContaDigito">
                    </div>
                </div>
                <div class="grid4Col">
                    <div class="form-group">
                        <label for="CodigoTransmissao">Código Transmissão</label>
                        <input type="text" id="CodigoTransmissao">
                    </div>
                    <div class="form-group">
                        <label for="Convenio">Convênio</label>
                        <input type="text" id="Convenio">
                    </div>
                    <div class="form-group">
                        <label for="Modalidade">Modalidade</label>
                        <input type="text" id="Modalidade">
                    </div>
                    <div class="form-group">
                        <label for="CodigoCedente">Código Cedente</label>
                        <input type="text" id="CodigoCedente">
                    </div>
                </div>
            </div>
            <div id="remessaRetorno" class="panelAba panelEsquerda-container">
                <div class="form-group">
                    <div class="grid2Col">
                        <div class="form-group">
                            <label for="LayoutRemessa">Layout CNAB</label>
                            <select id="LayoutRemessa">
                                <option value="0" selected>CNAB240</option>
                                <option value="1">CNAB400</option>
                            </select>
                        </div>
                        <div class="grid2Col">
                            <label for="LeCedenteRetorno">Lê Cedente Retorno</label>
                            <input type="checkbox" id="LeCedenteRetorno">
                            <br>
                            <br>
                        </div>
                    </div>
                </div>
                <div class="grid2Col">
                    <div class="form-group">
                        <label for="DirArqRemessa">Diretório Arquivo Remessa</label>
                        <input type="text" id="DirArqRemessa">
                    </div>
                    <div class="form-group">
                        <label for="NomeArqRemessa">Nome Arquivo Remessa</label>
                        <input type="text" id="NomeArqRemessa">
                    </div>
                </div>
                <div class="grid2Col">
                    <div class="form-group">
                        <label for="DirArqRetorno">Diretório Arquivo Retorno</label>
                        <input type="text" id="DirArqRetorno">
                    </div>
                    <div class="form-group">
                        <label for="NomeArqRetorno">Nome Arquivo Retorno</label>
                        <input type="text" id="NomeArqRetorno">
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
                </div>
            </div>
            <div id="webservices" class="panelAba panelConfiguracoes-container">
                <div class="grid2Col">
                    <label>Ambiente</label>
                    <div>
                        <input type="radio" id="producao" name="ambiente" value="0" checked>
                        <label for="producao">Produção</label>
                        <input type="radio" id="homologacao" name="ambiente" value="1">
                        <label for="homologacao">Homologação</label>
                    </div>
                </div>
                <br>
                <div class="grid4Col">
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
                        <label for="WSOperacao">Operação</label>
                        <select id="WSOperacao">
                            <option value="0" selected>Inclusão</option>
                            <option value="1">Alteração</option>
                            <option value="2">Baixa</option>
                            <option value="3">Consulta</option>
                            <option value="4">ConsultaDetalhada</option>
                            <option value="5">PIXCriar</option>
                            <option value="6">PIXCancelar</option>
                            <option value="7">PIXConsultar</option>
                            <option value="8">Cancelar</option>
                            <option value="9">Ticket</option>
                        </select>
                    </div>
                    <div class="grid2Col">
                        <div class="form-group">
                            <label for="VersaoDF">Versão</label>
                            <input type="text" id="VersaoDF">
                        </div>
                        <div class="form-group">
                            <label for="Timeout">Timeout</label>
                            <input type="number" id="Timeout" value="30000">
                        </div>
                    </div>
                </div>
                <div class="form-group">
                    <label for="ClientID">Client ID</label>
                    <input type="text" id="ClientID">
                </div>
                <div class="form-group">
                    <label for="ClientSecret">Client Secret</label>
                    <input type="text" id="ClientSecret">
                </div>
                <div class="grid2Col">
                    <div class="form-group">
                        <label for="KeyUser">Key User</label>
                        <input type="text" id="KeyUser">
                    </div>
                    <div class="form-group">
                        <label for="Scope">Scope</label>
                        <input type="text" id="Scope">
                    </div>
                </div>
                <div class="grid4Col">
                    <label for="IndicadorPix">Indicador Pix</label>
                    <input type="checkbox" id="IndicadorPix">
                </div>
                <br>
                <div class="grid2Col">
                    <div class="form-group">
                        <label for="ArquivoCRT">Arquivo CRT</label>
                        <input type="text" id="ArquivoCRT">
                    </div>
                    <div class="form-group">
                        <label for="ArquivoKEY">Arquivo KEY</label>
                        <input type="text" id="ArquivoKEY">
                    </div>
                </div>
                <div class="grid2Col">
                    <div class="form-group">
                        <label for="PathGravarRegistro">Path Log</label>
                        <input type="text" id="PathGravarRegistro">
                    </div>
                    <div class="grid2Col">
                        <div class="form-group">
                            <label for="NomeArquivoLog">Nome Log</label>
                            <input type="text" id="NomeArquivoLog">
                        </div>
                        <div class="form-group">
                            <label for="WSLogNivel">Log Nível</label>
                            <select id="WSLogNivel">
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
        <div class="buttons">
            <input type="button" id="carregarConfiguracoes" value="Carregar Configurações">
            <input type="button" id="salvarConfiguracoes" value="Salvar Configurações">
        </div>
    </div>
    <div class="cfgPanelDireita">
        <div class="tabsPanel">
            <div class="grid3Col">
                <input type="button" id="GerarRemessaStream" value="Gerar Remessa (Stream)">
                <input type="button" id="SalvarPDF" value="Salvar PDF (Stream)">
                <input type="button" id="LerRetornoStream" value="Ler Retorno (Stream)">
                <input type="button" id="EnviarEmail" value="Enviar Email">
                <input type="button" id="LinhaDigitavel" value="Linha Digitável">
                <input type="button" id="CodigoBarras" value="Codigo de Barras">
                <input type="button" id="ListaOcorrencias" value="Listar Ocorrencias">
                <input type="button" id="ListaOcorrenciasEX" value="Listar Ocorrencias Ex">
                <input type="button" id="ListaCaractTitulo" value="Listar Carac Titulo">
                <input type="button" id="CodigosMoraAceitos" value="Código Mora Aceitos">
                <input type="button" id="TamNossoNumero" value="Tamanho Nosso Numero">
                <input type="button" id="MontarNossoNumero" value="Montar Nosso Número">
                <input type="button" id="ListaBancos" value="Listar Bancos">
                <input type="button" id="LimparRepostas" value="Limpar Respostas">
            </div>
            <br>
            <div class="grid3Col">
                <input type="button" id="EnviarBoletoInclusao" value="Enviar Boleto (WebService)">
                <input type="button" id="EnviarBoletoConsultaDetalhada" value="Consulta Detalhe">
                <input type="button" id="ConsultarTitulosPorPeriodo" value="Consulta Lista Boletos">
                <input type="button" id="BaixaTitulo" value="Baixa Título">
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

                LogPath: $('#LogPath').val(),
                LogNivel: $('#LogNivel').val(),

                TipoInscricao: $('#TipoInscricao').val(),
                CNPJCPF: $('#CNPJCPF').val(),
                Nome: $('#Nome').val(),
                Logradouro: $('#Logradouro').val(),
                NumeroRes: $('#NumeroRes').val(),
                Bairro: $('#Bairro').val(),
                Complemento: $('#Complemento').val(),
                Cidade: $('#Cidade').val(),
                UF: $('#UF').val(),
                CEP: $('#CEP').val(),
                Telefone: $('#Telefone').val(),
                TipoDocumento: $('#TipoDocumento').val(),
                TipoCarteira: $('#TipoCarteira').val(),
                ResponEmissao: $('#ResponEmissao').val(),
                Agencia: $('#Agencia').val(),
                AgenciaDigito: $('#AgenciaDigito').val(),
                Conta: $('#Conta').val(),
                ContaDigito: $('#ContaDigito').val(),
                CodigoTransmissao: $('#CodigoTransmissao').val(),
                Convenio: $('#Convenio').val(),
                Modalidade: $('#Modalidade').val(),
                CodigoCedente: $('#CodigoCedente').val(),

                PrinterName: $('#PrinterName').val(),
                Layout: $('#Layout').val(),
                DirLogo: $('#DirLogo').val(),
                NumeroCopias: $('#NumeroCopias').val(),
                NomeArquivo: $('#NomeArquivo').val(),

                TipoCobranca: $('#TipoCobranca').val(),

                LayoutRemessa: $('#LayoutRemessa').val(),
                LeCedenteRetorno: $('#LeCedenteRetorno').prop('checked') ? 1 : 0,
                DirArqRemessa: $('#DirArqRemessa').val(),
                NomeArqRemessa: $('#NomeArqRemessa').val(),
                DirArqRetorno: $('#DirArqRetorno').val(),
                NomeArqRetorno: $('#NomeArqRetorno').val(),

                emailNome: $('#emailNome').val(),
                emailConta: $('#emailConta').val(),
                emailServidor: $('#emailServidor').val(),
                emailPorta: $('#emailPorta').val(),
                emailSSL: $('#emailSSL').prop('checked') ? 1 : 0,
                emailTLS: $('#emailTLS').prop('checked') ? 1 : 0,
                emailUsuario: $('#emailUsuario').val(),
                emailSenha: $('#emailSenha').val(),

                ambiente: parseInt($('input[name="ambiente"]:checked').val(), 10),
                WSOperacao: $('#WSOperacao').val(),
                SSLType: $('#SSLType').val(),
                VersaoDF: $('#VersaoDF').val(),
                Timeout: $('#Timeout').val(),
                ArquivoCRT: $('#ArquivoCRT').val(),
                ArquivoKEY: $('#ArquivoKEY').val(),
                PathGravarRegistro: $('#PathGravarRegistro').val(),
                NomeArquivoLog: $('#NomeArquivoLog').val(),
                WSLogNivel: $('#WSLogNivel').val(),

                SSLHttpLib: $('#SSLHttpLib').val(),

                ClientID: $('#ClientID').val(),
                ClientSecret: $('#ClientSecret').val(),
                KeyUser: $('#KeyUser').val(),
                Scope: $('#Scope').val(),
                IndicadorPix: $('#IndicadorPix').prop('checked') ? 1 : 0
            };

            chamaAjaxEnviar(infoData);
        });

        $('#GerarRemessaStream').on('click', function() {
            selecionarArquivo(".ini", function(AeArquivoIni) {
                chamaAjaxEnviar({
                    metodo: "GerarRemessaStream",
                    AeArquivoIni: AeArquivoIni
                });
            });
        });

        $('#SalvarPDF').on('click', function() {
            selecionarArquivo(".ini", function(AeArquivoIni) {
                chamaAjaxEnviar({
                    metodo: "SalvarPDF",
                    AeArquivoIni: AeArquivoIni
                });
            });
        });

        $('#LerRetornoStream').on('click', function() {
            selecionarArquivo("", function(AeArquivoIni) {
                chamaAjaxEnviar({
                    metodo: "LerRetornoStream",
                    AeArquivoIni: AeArquivoIni
                });
            });
        });

        $('#EnviarEmail').on('click', function() {
            selecionarArquivo("", function(AeArquivoIni) {
                inputBox("Digite o endereço do destinatário:", function(AePara) {
                    inputBox("Digite o assunto:", function(AeAssunto) {
                        inputBox("Digite a mensagem:", function(AeMensagem) {
                            chamaAjaxEnviar({
                                metodo: "EnviarEmail",
                                AeArquivoIni: AeArquivoIni,
                                AePara: AePara,
                                AeAssunto: AeAssunto,
                                AeCC: "",
                                AeMensagem: AeMensagem
                            });
                        });
                    });
                });
            });
        });

        $('#LinhaDigitavel').on('click', function() {
            selecionarArquivo("", function(AeArquivoIni) {
                chamaAjaxEnviar({
                    metodo: "RetornaLinhaDigitavel",
                    AeArquivoIni: AeArquivoIni
                });
            });
        });

        $('#CodigoBarras').on('click', function() {
            selecionarArquivo("", function(AeArquivoIni) {
                chamaAjaxEnviar({
                    metodo: "RetornaCodigoBarras",
                    AeArquivoIni: AeArquivoIni
                });
            });
        });

        $('#ListaOcorrencias').on('click', function() {
            chamaAjaxEnviar({
                metodo: "ListaOcorrencias",
                TipoCobranca: $('#TipoCobranca').val()
            });
        });

        $('#ListaOcorrenciasEX').on('click', function() {
            chamaAjaxEnviar({
                metodo: "ListaOcorrenciasEX",
                TipoCobranca: $('#TipoCobranca').val()
            });
        });

        $('#ListaCaractTitulo').on('click', function() {
            chamaAjaxEnviar({
                metodo: "ListaCaractTitulo",
                TipoCobranca: $('#TipoCobranca').val()
            });
        });

        $('#CodigosMoraAceitos').on('click', function() {
            chamaAjaxEnviar({
                metodo: "CodigosMoraAceitos",
                TipoCobranca: $('#TipoCobranca').val()
            });
        });

        $('#TamNossoNumero').on('click', function() {
            inputBox("Digite o nosso número:", function(nossoNumero) {
                chamaAjaxEnviar({
                    metodo: "TamNossoNumero",
                    TipoCobranca: $('#TipoCobranca').val(),
                    AeCarteira: $('#TipoCarteira').val(),
                    AenossoNumero: nossoNumero,
                    AeConvenio: $('#Convenio').val()
                });
            });
        });

        $('#MontarNossoNumero').on('click', function() {
            selecionarArquivo("", function(AeArquivoIni) {
                chamaAjaxEnviar({
                    metodo: "MontarNossoNumero",
                    AeArquivoIni: AeArquivoIni
                });
            });
        });

        $('#ListaBancos').on('click', function() {
            chamaAjaxEnviar({
                metodo: "ListaBancos"
            });
        });

        $('#LimparRepostas').on('click', function() {
            $.ajax(
                $('#result').val(""));
        });

        $('#EnviarBoletoInclusao').on('click', function() {
            selecionarArquivo("", function(AeArquivoIni) {
                chamaAjaxEnviar({
                    metodo: "EnviarBoleto",
                    AeArquivoIni: AeArquivoIni,
                    AeCodigoOperacao: 0
                });
            });
        });

        $('#EnviarBoletoConsultaDetalhada').on('click', function() {
            selecionarArquivo("", function(AeArquivoIni) {
                chamaAjaxEnviar({
                    metodo: "EnviarBoleto",
                    AeArquivoIni: AeArquivoIni,
                    AeCodigoOperacao: 4
                });
            });
        });

        $('#ConsultarTitulosPorPeriodo').on('click', function() {
            selecionarArquivo("", function(AeArquivoIni) {
                chamaAjaxEnviar({
                    metodo: "ConsultarTitulosPorPeriodo",
                    AeArquivoIni: AeArquivoIni
                });
            });
        });

        $('#BaixaTitulo').on('click', function() {
            selecionarArquivo("", function(AeArquivoIni) {
                chamaAjaxEnviar({
                    metodo: "EnviarBoleto",
                    AeArquivoIni: AeArquivoIni,
                    AeCodigoOperacao: 2
                });
            });
        });

        function chamaAjaxEnviar(infoData) {
            // Fazer a chamada passando o modo ST ou MT. 
            // Ex: http://localhost/NFe/ACBrBoletoBase.php?modo=MT
            // Ex: http://localhost/NFe/ACBrBoletoBase.php?modo=ST
            var modo = "<?php echo $modo; ?>";
            if (modo == "")
                modo = "MT";

            $.ajax({
                url: modo + '/ACBrBoletoServicos' + modo + '.php',
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

                $('#LogPath').val(response.dados.LogPath);
                $('#LogNivel').val(response.dados.LogNivel);

                $('#TipoInscricao').val(response.dados.TipoInscricao);
                $('#CNPJCPF').val(response.dados.CNPJCPF);
                $('#Nome').val(response.dados.Nome);
                $('#Logradouro').val(response.dados.Logradouro);
                $('#NumeroRes').val(response.dados.NumeroRes);
                $('#Bairro').val(response.dados.Bairro);
                $('#Complemento').val(response.dados.Complemento);
                $('#Cidade').val(response.dados.Cidade);
                $('#UF').val(response.dados.UF);
                $('#CEP').val(response.dados.CEP);
                $('#Telefone').val(response.dados.Telefone);
                $('#TipoDocumento').val(response.dados.TipoDocumento);
                $('#TipoCarteira').val(response.dados.TipoCarteira);
                $('#Agencia').val(response.dados.Agencia);
                $('#AgenciaDigito').val(response.dados.AgenciaDigito);
                $('#Conta').val(response.dados.Conta);
                $('#ContaDigito').val(response.dados.ContaDigito);
                $('#CodigoTransmissao').val(response.dados.CodigoTransmissao);
                $('#Convenio').val(response.dados.Convenio);
                $('#Modalidade').val(response.dados.Modalidade);
                $('#CodigoCedente').val(response.dados.CodigoCedente);

                $('#PrinterName').val(response.dados.PrinterName);
                $('#Layout').val(response.dados.Layout);
                $('#DirLogo').val(response.dados.DirLogo);
                $('#NumeroCopias').val(response.dados.NumeroCopias);
                $('#NomeArquivo').val(response.dados.NomeArquivo);

                $('#TipoCobranca').val(response.dados.TipoCobranca);

                $('#LayoutRemessa').val(response.dados.LayoutRemessa);
                $('#LeCedenteRetorno').prop('checked', response.dados.LeCedenteRetorno == 1);
                $('#DirArqRemessa').val(response.dados.DirArqRemessa);
                $('#NomeArqRemessa').val(response.dados.NomeArqRemessa);
                $('#DirArqRetorno').val(response.dados.DirArqRetorno);
                $('#NomeArqRetorno').val(response.dados.NomeArqRetorno);

                $('#emailNome').val(response.dados.emailNome);
                $('#emailConta').val(response.dados.emailConta);
                $('#emailServidor').val(response.dados.emailServidor);
                $('#emailPorta').val(response.dados.emailPorta);
                $('#emailSSL').prop('checked', response.dados.emailSSL == 1);
                $('#emailTLS').prop('checked', response.dados.emailTLS == 1);
                $('#emailUsuario').val(response.dados.emailUsuario);
                $('#emailSenha').val(response.dados.emailSenha);

                $('input[name="ambiente"][value="' + response.dados.ambiente + '"]').prop('checked', true);
                $('#WSOperacao').val(response.dados.WSOperacao);
                $('#SSLType').val(response.dados.SSLType);
                $('#VersaoDF').val(response.dados.VersaoDF);
                $('#Timeout').val(response.dados.Timeout);
                $('#ArquivoCRT').val(response.dados.ArquivoCRT);
                $('#ArquivoKEY').val(response.dados.ArquivoKEY);
                $('#PathGravarRegistro').val(response.dados.PathGravarRegistro);
                $('#NomeArquivoLog').val(response.dados.NomeArquivoLog);
                $('#WSLogNivel').val(response.dados.WSLogNivel);

                $('#SSLHttpLib').val(response.dados.SSLHttpLib);

                $('#ClientID').val(response.dados.ClientID);
                $('#ClientSecret').val(response.dados.ClientSecret);
                $('#KeyUser').val(response.dados.KeyUser);
                $('#Scope').val(response.dados.Scope);
                $('#IndicadorPix').prop('checked', response.dados.IndicadorPix == 1);
            } else {
                processaResponseGeral(response);
            }
        }
    </script>
</body>

</html>