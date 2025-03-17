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
    <meta name="ACBrReinf" content="width=device-width, initial-scale=1.0">
    <title>
        <?php
        $title = 'ACBrReinf';
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
                <div class="grid2Col">
                    <button class="selecionado" onclick="ativaAba(event, 'configuracoes', 'panelEsquerda')">Configurações</button>
                </div>
            </div>
            <div id="configuracoes" class="panelAba selecionado panelEsquerda-container">
                <div class="tabsPanel">
                    <div class="tabAbas">
                        <button class="selecionado" onclick="ativaAba(event, 'geral', 'panelConfiguracoes')">Geral</button>
                        <button onclick="ativaAba(event, 'webservices', 'panelConfiguracoes')">WebServices</button>
                        <button onclick="ativaAba(event, 'certificados', 'panelConfiguracoes')">Certificados</button>
                        <button onclick="ativaAba(event, 'arquivos', 'panelConfiguracoes')">Arquivos</button>
                        <button onclick="ativaAba(event, 'contribuinte', 'panelConfiguracoes')">Contribuinte</button>
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
                        <div class="form-group">
                            <label for="pathSchemas">Pasta dos Schemas</label>
                            <input type="text" id="pathSchemas">
                        </div>
                    </div>
                    <div id="webservices" class="panelAba panelConfiguracoes-container">
                        <div class="grid3Col">
                            <div class="form-group">
                                <label for="UF">UF do Emitente</label>
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
                                <option value="1" selected>cryOpenSSL</option>
                                <option value="2">cryCapicom</option>
                                <option value="3">cryWinCrypt</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label for="SSLHttpLib">HttpLib</label>
                            <select id="SSLHttpLib">
                                <option value="0">httpNone</option>
                                <option value="1">httpWinINet</option>
                                <option value="2">httpWinHttp</option>
                                <option value="3" selected>httpOpenSSL</option>
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
                            <br>
                            <div class="grid3Col">
                                <input type="button" id="ObterCertificados" value="ObterCertificados">
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
                            <label for="SepararPorCNPJ">Separar Arqs pelo CNPJ do Certificado</label>
                            <input type="checkbox" id="SepararPorCNPJ">
                        </div>
                        <div class="grid2Col">
                            <label for="SepararPorModelo">Separar Arqs pelo Modelo do Documento</label>
                            <input type="checkbox" id="SepararPorModelo">
                        </div>
                        <br>
                        <div class="form-group">
                            <label for="PathReinf">Pasta Arquivos Reinf</label>
                            <input type="text" id="PathReinf">
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
                    <div id="contribuinte" class="panelAba panelConfiguracoes-container">
                        <div class="form-group">
                            <label for="IdContribuinte">CNPJ ou CPF do Contribuinte</label>
                            <input type="text" id="IdContribuinte">
                        </div>
                        <div class="form-group">
                            <label for="IdTransmissor">CNPJ ou CPF do Transmissor</label>
                            <input type="text" id="IdTransmissor">
                        </div>
                        <div class="form-group">
                            <label for="TipoContribuinte">Tipo de Contribuinte</label>
                            <select id="TipoContribuinte" name="TipoContribuinte">
                                <option value="0" selected>tcPessoaJuridica</option>
                                <option value="1">tcOrgaoPublico</option>
                                <option value="2">tcPessoaFisica</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label for="versaoDF">Versão Documento Fiscal</label>
                            <select id="versaoDF" name="versaoDF">
                                <option value="0">v1_03_00</option>
                                <option value="1">v1_03_02</option>
                                <option value="2">v1_04_00</option>
                                <option value="3">v1_05_00</option>
                                <option value="4">v1_05_01</option>
                                <option value="5">v2_01_01</option>
                                <option value="6" selected>v2_01_02</option>
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
            <div class="tabAbas">
                <div class="grid2Col">
                    <button class="selecionado" onclick="ativaAba(event, 'comandos', 'panelDireita')">Comandos</button>
                </div>
            </div>
            <div id="comandos" class="panelAba selecionado button-container">
                <div class="grid3Col">
                    <input type="button" id="CriarEnviarReinfIni" value="Criar e Enviar Reinf (ini)">
                    <input type="button" id="CriarEnviarReinfXml" value="Criar e Enviar Reinf (xml)">
                    <input type="button" id="ConsultarProtocolo" value="Consultar Protocolo">
                    <input type="button" id="ConsultarReciboReinf" value="Consultar Recibo">
                    <input type="button" id="ValidarXml" value="Validar Xml (não assinado)">
                </div>
            </div>
        </div>
        <div class="grid1Col">
            <label for="result">Respostas</label>
            <textarea id="result" rows="50" readonly></textarea>
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
                LogPath: $('#LogPath').val(),
                LogNivel: $('#LogNivel').val(),

                metodo: "salvarConfiguracoes",
                exibirErroSchema: $('#exibirErroSchema').prop('checked') ? 1 : 0,
                formatoAlerta: $('#formatoAlerta').val(),
                formaEmissao: $('#formaEmissao').val(),
                versaoDF: $('#versaoDF').val(),
                retirarAcentos: $('#retirarAcentos').prop('checked') ? 1 : 0,
                SalvarGer: $('#SalvarGer').prop('checked') ? 1 : 0,
                pathSalvar: $('#pathSalvar').val(),
                pathSchemas: $('#pathSchemas').val(),
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
                SepararPorCNPJ: $('#SepararPorCNPJ').prop('checked') ? 1 : 0,
                SepararPorModelo: $('#SepararPorModelo').prop('checked') ? 1 : 0,
                PathReinf: $('#PathReinf').val(),
                IdContribuinte: $('#IdContribuinte').val(),
                IdTransmissor: $('#IdTransmissor').val(),
                TipoContribuinte: $('#TipoContribuinte').val(),

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
                NumeroSerie: $('#NumeroSerie').val()
            };

            chamaAjaxEnviar(infoData);
        });

        $('#OpenSSLInfo').on('click', function() {
            chamaAjaxEnviar({
                metodo: "OpenSSLInfo"
            });
        });

        $('#ObterCertificados').on('click', function() {
            chamaAjaxEnviar({
                metodo: "ObterCertificados"
            });
        });

        $('#CriarEnviarReinfIni').on('click', function() {
            selecionarArquivo(".ini", function(eArqIni) {
                chamaAjaxEnviar({
                    metodo: "CriarEnviarReinfIni",
                    eArqIni: eArqIni
                });
         xml});
        });

        $('#CriarEnviarReinfXml').on('click', function() {
            selecionarArquivo(".xml", function(eArquivoOuXML) {
                chamaAjaxEnviar({
                    metodo: "CriarEnviarReinfXml",
                    eArquivoOuXML: eArquivoOuXML
                });
         xml});
        });

        $('#ConsultarProtocolo').on('click', function() {
            inputBox("Digite o Número do Protocolo:", function(eProtocolo) {
                chamaAjaxEnviar({
                    metodo: "ConsultarProtocolo",
                    eProtocolo: eProtocolo
                });
            });
        });

        var periodoAtual = "<?php echo date('Y/m'); ?>";        
        var dataAtual = "<?php echo date('d/m/Y'); ?>";        
        $('#ConsultarReciboReinf').on('click', function() {
            inputBox("Período de Apuração (aaaa/mm):", function(ePerApur) {
                inputBox("Tipo do Evento (R-xxxx):", function(aTipoEvento) {
                    inputBox("Nr. Inscrição do Estabelecimento:", function(eNrInscEstab) {
                        inputBox("Nr. CNPJ do Prestador de Serviço:", function(eCnpjPrestador) {
                            inputBox("Nr. Inscrição do Tomador:", function(eNrInscTomador) {
                                inputBox("Data de Apuração (DD/MM/AAAA):", function(eDtApur) {
                                    inputBox("Nr. Inscrição do Beneficiário:", function(eCpfCnpjBenef) {
                                        inputBox("Nr. CNPJ da Fonte Pagadora:", function(eCnpjFonte) {
                                            chamaAjaxEnviar({
                                                metodo: "ConsultarReciboReinf",
                                                ePerApur: ePerApur,
                                                aTipoEvento: aTipoEvento,
                                                eNrInscEstab: eNrInscEstab,
                                                eCnpjPrestador: eCnpjPrestador,
                                                eNrInscTomador: eNrInscTomador,
                                                eDtApur: eDtApur,
                                                eCpfCnpjBenef: eCpfCnpjBenef,
                                                eCnpjFonte: eCnpjFonte
                                            });
                                        }, "", false);
                                    }, "", false);
                                }, dataAtual);
                            }, "", false);
                        }, "", false);
                    }, $('#IdContribuinte').val());
                }, "1000");
            }, periodoAtual);
        });

        $('#ValidarXml').on('click', function() {
            selecionarArquivo(".xml", function(eArquivoXML) {
                chamaAjaxEnviar({
                    metodo: "ValidarXml",
                    eArquivoXML: eArquivoXML
                });
         xml});
        });

        function chamaAjaxEnviar(infoData) {
            // Fazer a chamada passando o modo ST ou MT. 
            // Ex: http://localhost/Reinf/ACBrReinfBase.php?modo=MT
            // Ex: http://localhost/Reinf/ACBrReinfBase.php?modo=ST
            var modo = "<?php echo $modo; ?>";
            if (modo == "")
                modo = "MT";

            $.ajax({
                url: modo + '/ACBrReinfServicos' + modo + '.php',
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

        function inputBox(mensagem, callback, padrao = "", obrigatorio = true) {
            var ACodigoUF = prompt(mensagem, padrao);
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

                $('#exibirErroSchema').prop('checked', response.dados.exibirErroSchema == 1);
                $('#formatoAlerta').val(response.dados.formatoAlerta);
                $('#formaEmissao').val(response.dados.formaEmissao);
                $('#versaoDF').val(response.dados.versaoDF);
                $('#retirarAcentos').prop('checked', response.dados.retirarAcentos == 1);
                $('#SalvarGer').prop('checked', response.dados.SalvarGer == 1);
                $('#pathSalvar').val(response.dados.pathSalvar);
                $('#pathSchemas').val(response.dados.pathSchemas);
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
                $('#SalvarEvento').prop('checked', response.dados.SalvarEvento == 1);
                $('#SepararPorCNPJ').prop('checked', response.dados.SepararPorCNPJ == 1);
                $('#SepararPorModelo').prop('checked', response.dados.SepararPorModelo == 1);
                $('#PathReinf').val(response.dados.PathReinf);
                $('#IdContribuinte').val(response.dados.IdContribuinte);
                $('#IdTransmissor').val(response.dados.IdTransmissor);
                $('#TipoContribuinte').val(response.dados.TipoContribuinte);

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
            } else {
                processaResponseGeral(response);
            }
        }
    </script>
</body>

</html>