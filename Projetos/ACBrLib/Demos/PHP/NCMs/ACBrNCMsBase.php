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
    <meta name="ACBrNCMs" content="width=device-width, initial-scale=1.0">
    <title>
        <?php
        $title = 'ACBrNCMs';
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

        .cfgPanelEsquerda {
            width: 25%;
            padding: 10px;
            box-sizing: border-box;
        }

        .cfgPanelDireita {
            width: 75%;
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
            <div class="grid4Col">
                <div class="tabAbas">
                    <button class="selecionado" onclick="ativaAba(event, 'comandos', 'panelEsquerda')">Comandos</button>
                </div>
            </div>
            <div id="comandos" class="panelAba selecionado panelEsquerda-container">
                <div class="tabsPanel">
                    <div class="grid1Col">
                        <input type="button" id="ObterNCMs" value="Obter NCMs">
                    </div>
                    <br>
                    <div class="grid1Col">
                        <input type="button" id="BaixarLista" value="Baixar Lista NCMs">
                    </div>
                    <br>
                    <label>Filtrar por Código</label>
                    <div class="form-group">
                        <input type="text" id="txtBuscarPorCodigo">
                    </div>
                    <div class="grid1Col">
                        <input type="button" id="btnBuscarPorCodigo" value="Filtrar">
                    </div>
                    <br>
                    <label>Filtrar por Descrição</label>
                    <div class="form-group">
                        <input type="text" id="txtBuscarPorDescricao">
                    </div>
                    <div class="form-group">
                        <select id="TipoFiltroDescricao">
                            <option value="0">Inicia com...</option>
                            <option value="1" selected selected>Contém</option>
                            <option value="2">Finaliza com...</option>
                        </select>
                    </div>
                    <div class="grid1Col">
                        <input type="button" id="btnBuscarPorDescricao" value="Filtrar">
                    </div>
                    <br>
                    <label>Validar NCM</label>
                    <div class="form-group">
                        <input type="text" id="txtValidar">
                    </div>
                    <div class="grid1Col">
                        <input type="button" id="btnValidar" value="Validar">
                    </div>
                    <br>
                    <div class="form-group">
                        <label for="LogPath">Path Log</label>
                        <input type="text" id="LogPath">
                    </div>
                    <div class="grid2Col">
                        <div class="form-group">
                            <label for="LogNivel">Log Nível</label>
                            <select id="LogNivel">
                                <option value="0">logNenhum</option>
                                <option value="1">logSimples</option>
                                <option value="2">logNormal</option>
                                <option value="3">logCompleto</option>
                                <option value="4" selected>logParanoico</option>
                            </select>
                        </div>
                    </div>
                </div>
                <div class="grid1Col">
                    <input type="button" id="carregarConfiguracoes" value="Carregar Configurações">
                    <input type="button" id="salvarConfiguracoes" value="Salvar Configurações">
                </div>
            </div>
        </div>
    </div>
    <div class="cfgPanelDireita">
        <div class="grid1Col">
            <label for="result">Respostas</label>
            <textarea id="result" rows="30" readonly></textarea>
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
                LogPath: $('#LogPath').val(),
                LogNivel: $('#LogNivel').val(),

                metodo: "salvarConfiguracoes"
            };

            chamaAjaxEnviar(infoData);
        });

        $('#ObterNCMs').on('click', function() {
            chamaAjaxEnviar({
                metodo: "ObterNCMs"
            });
        });

        $('#BaixarLista').on('click', function() {
            inputBox("Digite caminho local do servidor e nome do arquivo:", function(cNomeArquivo) {
                chamaAjaxEnviar({
                    metodo: "BaixarLista",
                    cNomeArquivo: cNomeArquivo
                });
            });
        });

        $('#btnBuscarPorCodigo').on('click', function() {
            chamaAjaxEnviar({
                metodo: "BuscarPorCodigo",
                cNCM: $('#txtBuscarPorCodigo').val()
            });
        });

        $('#btnBuscarPorDescricao').on('click', function() {
            chamaAjaxEnviar({
                metodo: "BuscarPorDescricao",
                cDesc: $('#txtBuscarPorDescricao').val(),
                nTipo: $('#TipoFiltroDescricao').val(),
            });
        });

        $('#btnValidar').on('click', function() {
            chamaAjaxEnviar({
                metodo: "Validar",
                cNCM: $('#txtValidar').val()
            });
        });

        function chamaAjaxEnviar(infoData) {
            // Fazer a chamada passando o modo ST ou MT. 
            // Ex: http://localhost/NCMs/ACBrNCMsBase.php?modo=MT
            // Ex: http://localhost/NCMs/ACBrNCMsBase.php?modo=ST
            var modo = "<?php echo $modo; ?>";
            if (modo == "")
                modo = "MT";

            $.ajax({
                url: modo + '/ACBrNCMsServicos' + modo + '.php',
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
            } else {
                processaResponseGeral(response);
            }
        }
    </script>
</body>

</html>