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
    <title>
        <?php
        $title = 'ACBrIBGE';
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
        .tituloColunas {
            display: flex;
            align-items: center;
        }

        .tituloColunas img {
            width: 5%;
            height: auto;
            margin-right: 20px;
        }

        .configcoluna {
            display: flex;
            flex-direction: column;
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

        .frame200 {
            border: 1px solid #ccc;
            padding: 20px;
            margin: 10px;
            border-radius: 10px;
            background-color: #f9f9f9;
            width: 200px;
        }

        .frame600 {
            border: 1px solid #ccc;
            padding: 20px;
            margin: 10px;
            border-radius: 10px;
            background-color: #f9f9f9;
            width: 600px;
        }

        .container {
            display: flex;
        }
    </style>
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
</head>

<body>
    <div class="tituloColunas">
        <img src="https://svn.code.sf.net/p/acbr/code/trunk2/Exemplos/ACBrTEFD/Android/ACBr_96_96.png" alt="ACBr Logo">
        <h1>ACBrIBGE - MultiThread</h1>
    </div>
    <form id="formConsulta">
        <div class="container">
            <div class="frame200">
                <div class="grid3Col">
                    <div class="configcoluna">
                        <label for="ACodMun">Cód. Município</label>
                        <input type="text" id="ACodMun" name="ACodMun">
                        <br>
                        <input type="button" id="consultaPorCodigo" value="Consultar">
                    </div>
                </div>
            </div>
            <div class="frame600">
                <div class="grid3Col">
                    <div class="configcoluna">
                        <label for="cidadecons">Cidade</label>
                        <input type="text" id="cidadecons" name="cidadecons">
                    </div>
                    <div class="configcoluna">
                        <label for="ufcons">UF</label>
                        <input type="text" id="ufcons" name="ufcons">
                    </div>
                    <div class="configcoluna">
                        <label for="IgnorarCaixaEAcentos">Ignorar caixa e acentos</label>
                        <input type="checkbox" id="IgnorarCaixaEAcentos">
                    </div>
                </div>
                <br>
                <div class="grid3Col">
                    <div class="configcoluna">
                        <input type="button" id="consultaPorNome" value="Consultar">
                    </div>
                    <div class="configcoluna">
                        <input type="button" id="salvarConfiguracoes" value="Salvar Configurações">
                    </div>
                    <div class="configcoluna">
                        <input type="button" id="carregarConfiguracoes" value="Carregar Configurações">
                    </div>
                </div>
            </div>
        </div>
    </form>

    <label for="result">Retorno:</label>
    <br>
    <textarea id="result" rows="10" cols="120" readonly></textarea>
    <br><br>

    <?php
    $modo = isset($_GET['modo']) ? $_GET['modo'] : null;
    ?>

    <script>
        document.querySelector('.tituloColunas h1').textContent = document.title;

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
                IgnorarCaixaEAcentos: $('#IgnorarCaixaEAcentos').prop('checked') ? 1 : 0
            };

            chamaAjaxEnviar(infoData);
        });

        $('#consultaPorCodigo').on('click', function() {
            chamaAjaxEnviar({
                metodo: "BuscarPorCodigo",
                ACodMun: $('#ACodMun').val(),
                cidadecons: "",
                ufcons: "",
                IgnorarCaixaEAcentos: $('#IgnorarCaixaEAcentos').prop('checked') ? 1 : 0
            });
        });

        $('#consultaPorNome').on('click', function() {
            chamaAjaxEnviar({
                metodo: "BuscarPorNome",
                ACodMun: "",
                cidadecons: $('#cidadecons').val(),
                ufcons: $('#ufcons').val(),
                IgnorarCaixaEAcentos: $('#IgnorarCaixaEAcentos').prop('checked') ? 1 : 0
            });
        });

        function chamaAjaxEnviar(infoData) {
            // Fazer a chamada passando o modo ST ou MT. 
            // Ex: http://localhost/IBGE/ACBrIBGEBase.php?modo=MT
            // Ex: http://localhost/IBGE/ACBrIBGEBase.php?modo=ST
            var modo = "<?php echo $modo; ?>";
            if (modo == "")
                modo = "MT";

            $.ajax({
                url: modo + '/ACBrIBGEServicos' + modo + '.php',
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

        function processaResponseGeral(retorno) {
            if (retorno.mensagem)
                $('#result').val(retorno.mensagem)
            else
                $('#result').val('Erro: ' + JSON.stringify(retorno, null, 4));
        }

        function processaRetornoConfiguracoes(response) {
            if (response.dados) {
                if (response.dados) {
                    $('#result').val(JSON.stringify(response, null, 4));
                    $('#IgnorarCaixaEAcentos').prop('checked', response.dados.IgnorarCaixaEAcentos == 1);
                } else {
                    if (response.mensagem)
                        $('#result').val(response.mensagem)
                    else
                        $('#result').val('Erro: ' + JSON.stringify(response, null, 4));
                }
            } else {
                processaResponseGeral(response);
            }
        }
    </script>
</body>

</html>