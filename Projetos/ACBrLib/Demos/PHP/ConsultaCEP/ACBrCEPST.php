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
    <title>ACBrCEP</title>
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
        .frame350 {
            border: 1px solid #ccc;
            padding: 20px;
            margin: 10px;
            border-radius: 10px;
            background-color: #f9f9f9;
            width: 350px;
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
            justify-content: space-between;
        }
    </style>
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
</head>
<body>
    <div class="tituloColunas">
        <img src="https://svn.code.sf.net/p/acbr/code/trunk2/Exemplos/ACBrTEFD/Android/ACBr_96_96.png" alt="ACBr Logo">
        <h1>ACBrCEP - SingleThread</h1>
    </div>
    <form id="formConsulta">
        <div class="container">
            <div class="frame200">
                <div class="grid3Col">
                    <div class="configcoluna">
                        <label for="cepcons">Digite o CEP</label>
                        <input type="text" id="cepcons" name="cepcons">
                        <br>
                        <input type="button" id="consultaCEP" value="Consultar">
                    </div>
                </div>
            </div>
            <div class="frame600">
                <div class="grid3Col">
                    <div class="configcoluna">
                        <label for="tipocons">Tipo</label>
                        <input type="text" id="tipocons" name="tipocons">
                    </div>
                    <div class="configcoluna">
                        <label for="logradourocons">Logradouro</label>
                        <input type="text" id="logradourocons" name="logradourocons">
                    </div>
                    <div class="configcoluna">
                        <label for="bairrocons">Bairro</label>
                        <input type="text" id="bairrocons" name="bairrocons">
                    </div>
                    <div class="configcoluna">
                        <label for="cidadecons">Cidade</label>
                        <input type="text" id="cidadecons" name="cidadecons">
                    </div>
                    <div class="configcoluna">
                        <label for="ufcons">UF</label>
                        <input type="text" id="ufcons" name="ufcons">
                    </div>
                </div>
                <br>
                <div class="configcoluna">
                    <input type="button" id="consultalogradouro" value="Consultar">
                </div>
            </div>
            <div class="frame350">
                <div class="grid2Col">
                    <div class="configcoluna">
                        <label for="webservice">Selecione o WebService</label>
                        <select id="webservice" name="webservice">
                            <option value="0">wsNenhum</option>
                            <option value="1">wsBuscarCep</option>
                            <option value="2">wsCepLivre</option>
                            <option value="3">wsRepublicaVirtual</option>
                            <option value="4">wsBases4you</option>
                            <option value="5">wsRNSolucoes</option>
                            <option value="6">wsKingHost</option>
                            <option value="7">wsByJG</option>
                            <option value="8">wsCorreios</option>
                            <option value="9">wsDevMedia</option>
                            <option value="10">wsViaCep</option>
                            <option value="11">wsCorreiosSIGEP</option>
                            <option value="12">wsCepAberto</option>
                            <option value="13">wsWSCep</option>
                            <option value="14">wsOpenCep</option>
                            <option value="15">wsBrasilAPI</option>
                        </select>
                    </div>
                    <div class="configcoluna">
                        <label for="usuario">Usuário</label>
                        <input type="text" id="usuario" name="usuario">
                    </div>
                    <div class="configcoluna">
                        <label for="chaveacesso">Chave de Acesso</label>
                        <input type="password" id="chaveacesso" name="chaveacesso">
                    </div>
                    <div class="configcoluna">
                        <label for="senha">Senha</label>
                        <input type="password" id="senha" name="senha">
                    </div>
                    <div class="configcoluna">
                    <br>
                        <input type="button" id="salvarConfiguracoes" value="Salvar Configurações">
                    </div>
                    <div class="configcoluna">
                    <br>
                        <input type="button" id="carregarConfiguracoes" value="Carregar Configurações">
                    </div>
                </div>
            </div>
        </div>
    </form>
    <h2>Detalhes do Endereço</h2>
    <div class="grid4Col">
      <div class="configcoluna">
        <label for="tipologradouro">Tipo de Logradouro</label>
        <input type="text" id="tipologradouro" name="tipologradouro"><br>
      </div>
      <div class="configcoluna">
        <label for="logradouro">Logradouro</label>
        <input type="text" id="logradouro" name="logradouro"><br>
      </div>
      <div class="configcoluna">
        <label for="complemento">Complemento</label>
        <input type="text" id="complemento" name="complemento"><br>
      </div>
      <div class="configcoluna">
        <label for="bairro">Bairro</label>
        <input type="text" id="bairro" name="bairro"><br>
      </div>
      <div class="configcoluna">
        <label for="cep">CEP</label>
        <input type="text" id="cep" name="cep"><br>
      </div>
      <div class="configcoluna">
        <label for="ibgemunicipio">Município IBGE</label>
        <input type="text" id="ibgemunicipio" name="ibgemunicipio"><br>
      </div>
      <div class="configcoluna">
        <label for="ibgeuf">UF IBGE</label>
        <input type="text" id="ibgeuf" name="ibgeuf"><br>
      </div>
      <div class="configcoluna">
        <label for="municipio">Município</label>
        <input type="text" id="municipio" name="municipio"><br>
      </div>
      <div class="configcoluna">
        <label for="uf">UF</label>
        <input type="text" id="uf" name="uf"><br>
      </div>
    </div>

    <label for="result">Retorno:</label>
    <br>
    <textarea id="result" rows="10" cols="100" readonly></textarea>
    <br><br>
    <script>
        $(document).ready(function(){
            $('#consultaCEP').on('click', function() {
                $.ajax({
                    url: 'ST/consultaCEP.php',
                    type: 'POST',
                    data: {
                        metodocons: "BuscarPorCEP",
                        cepcons: $('#cepcons').val(),
                        tipocons: "",
                        logradourocons: "",
                        bairrocons: "",
                        cidadecons: "",
                        ufcons: "",
                        webservice: $('#webservice').val()
                    },
                    success: function(response) {
                        if(response.dados) {
                            $('#result').val(JSON.stringify(response, null, 4));
                            $('#tipologradouro').val(response.dados.tipologradouro || '');
                            $('#logradouro').val(response.dados.logradouro || '');
                            $('#complemento').val(response.dados.complemento || '');
                            $('#bairro').val(response.dados.bairro);
                            $('#cep').val(response.dados.cep);
                            $('#ibgemunicipio').val(response.dados.ibgemunicipio || '');
                            $('#ibgeuf').val(response.dados.ibgeuf || '');
                            $('#municipio').val(response.dados.municipio || '');
                            $('#uf').val(response.dados.UF || '');
                        } else {
                            if(response.mensagem)
                                $('#result').val(response.mensagem)
                            else
                                $('#result').val('Erro: ' + JSON.stringify(response, null, 4));
                        }
                    },
                    error: function(error) {
                        if(error.mensagem)
                            $('#result').val(error.mensagem)
                        else
                            $('#result').val('Erro: ' + JSON.stringify(error, null, 4));
                    }
                });
            });

            $('#consultalogradouro').on('click', function() {
                $.ajax({
                    url: 'ST/consultaCEP.php',
                    type: 'POST',
                    data: {
                        metodocons: "BuscarPorLogradouro",
                        cepcons: "",
                        tipocons: $('#tipocons').val(),
                        logradourocons: $('#logradourocons').val(),
                        bairrocons: $('#bairrocons').val(),
                        cidadecons: $('#cidadecons').val(),
                        ufcons: $('#ufcons').val(),
                        webservice: $('#webservice').val()
                    },
                    success: function(response) {
                        if(response.dados) {
                            $('#result').val(JSON.stringify(response, null, 4));
                            $('#tipologradouro').val(response.dados.tipologradouro || '');
                            $('#logradouro').val(response.dados.logradouro || '');
                            $('#complemento').val(response.dados.complemento || '');
                            $('#bairro').val(response.dados.bairro);
                            $('#cep').val(response.dados.cep);
                            $('#ibgemunicipio').val(response.dados.ibgemunicipio || '');
                            $('#ibgeuf').val(response.dados.ibgeuf || '');
                            $('#municipio').val(response.dados.municipio || '');
                            $('#uf').val(response.dados.UF || '');
                        } else {
                            if(response.mensagem)
                                $('#result').val(response.mensagem)
                            else
                                $('#result').val('Erro: ' + JSON.stringify(response, null, 4));
                        }
                    },
                    error: function(error) {
                        if(error.mensagem)
                            $('#result').val(error.mensagem)
                        else
                            $('#result').val('Erro: ' + JSON.stringify(error, null, 4));
                    }
                });
            });

            $.ajax({
                url: 'ST/carregarConfiguracoes.php',
                type: 'POST',
                success: function(response) {
                    if (response.dados) {
                        $('#result').val(JSON.stringify(response, null, 4));
                        $('#usuario').val(response.dados.usuario);
                        $('#senha').val(response.dados.senha);
                        $('#chaveacesso').val(response.dados.chaveacesso);
                        $('#webservice').val(response.dados.webservice);
                    } else {
                        if (response.mensagem)
                            $('#result').val(response.mensagem)
                        else
                            $('#result').val('Erro: ' + JSON.stringify(response, null, 4));
                    }
                },
                error: function(error) {
                    if (error.mensagem)
                        $('#result').val(error.mensagem)
                    else
                        $('#result').val('Erro: ' + JSON.stringify(error, null, 4));
                }
            });

            $('#carregarConfiguracoes').on('click', function() {
                $.ajax({
                    url: 'ST/carregarConfiguracoes.php',
                    type: 'POST',
                    success: function(response) {
                        if(response.dados) {
                            $('#result').val(JSON.stringify(response, null, 4));
                            $('#usuario').val(response.dados.usuario);
                            $('#senha').val(response.dados.senha);
                            $('#chaveacesso').val(response.dados.chaveacesso);
                            $('#webservice').val(response.dados.webservice);
                        } else {
                            if(response.mensagem)
                                $('#result').val(response.mensagem)
                            else
                                $('#result').val('Erro: ' + JSON.stringify(response, null, 4));
                        }
                    },
                    error: function(error) {
                        if(error.mensagem)
                            $('#result').val(error.mensagem)
                        else
                            $('#result').val('Erro: ' + JSON.stringify(error, null, 4));
                    }
                });
            });

            $('#salvarConfiguracoes').on('click', function() {
                $.ajax({
                    url: 'ST/salvarConfiguracoes.php',
                    type: 'POST',
                    data: {
                        webservice: $('#webservice').val(),
                        usuario: $('#usuario').val(),
                        senha: $('#senha').val(),
                        chaveacesso: $('#chaveacesso').val()
                    },
                    success: function(response) {
                        if(response.mensagem)
                            $('#result').val(response.mensagem)
                        else
                            $('#result').val('Erro: ' + JSON.stringify(response, null, 4));
                    },
                    error: function(error) {
                        if(error.mensagem)
                            $('#result').val(error.mensagem)
                        else
                            $('#result').val('Erro: ' + JSON.stringify(error, null, 4));
                    }
                });
            });
        });
    </script>
</body>
</html>
