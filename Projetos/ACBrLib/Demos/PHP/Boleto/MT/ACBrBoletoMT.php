<?php
/* {******************************************************************************}
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
*/
header('Content-Type: application/json; charset=UTF-8');

function Inicializar(&$handle, $ffi, $iniPath)
{
    $retorno = $ffi->Boleto_Inicializar(FFI::addr($handle), $iniPath, "");

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao inicializar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function Finalizar($handle, $ffi)
{
    $retorno = $ffi->Boleto_Finalizar($handle->cdata);

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao finalizar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function ConfigLer($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->Boleto_ConfigLer($handle->cdata, $eArqConfig);
    $sMensagem = FFI::new("char[535]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o arquivo ini. ") != 0)
        return -10;

    return 0;
}

function ConfigLerValor($handle, $ffi, $eSessao, $eChave, &$sValor)
{
    $sResposta = FFI::new("char[9048]");
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $retorno = $ffi->Boleto_ConfigLerValor($handle->cdata, $eSessao, $eChave, $sResposta, FFI::addr($esTamanho));

    $sMensagem = FFI::new("char[535]");

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao ler valor na secao[$eSessao] e chave[$eChave]. ", 1) != 0)
            return -10;
    }

    $sValor = FFI::string($sResposta);
    return 0;
}

function ConfigGravarValor($handle, $ffi, $eSessao, $eChave, $value)
{
    $retorno = $ffi->Boleto_ConfigGravarValor($handle->cdata, $eSessao, $eChave, $value);
    $sMensagem = FFI::new("char[535]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gravar valores [$value] na secao[$eSessao] e chave[$eChave]. ") != 0)
        return -10;

    return 0;
}

function ConfigGravar($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->Boleto_ConfigGravar($handle->cdata, $eArqConfig);
    $sMensagem = FFI::new("char[535]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gravar as configurações. ") != 0)
        return -10;

    return 0;
}

function UltimoRetorno($handle, $ffi, $retornolib, &$sMensagem, $msgErro, $retMensagem = 0)
{
    if (($retornolib !== 0) || ($retMensagem == 1)) {
        $esTamanho = FFI::new("long");
        $esTamanho->cdata = 9048;
        $ffi->Boleto_UltimoRetorno($handle->cdata, $sMensagem, FFI::addr($esTamanho));

        if ($retornolib !== 0) {
            $ultimoRetorno = FFI::string($sMensagem);
            $retorno = "$msgErro Código de erro: $retornolib. ";

            $retorno = $retorno;

            if ($ultimoRetorno != "") {
                $retorno = $retorno . "Último retorno: " . $ultimoRetorno;
            }

            echo json_encode(["mensagem" => $retorno]);
            return -10;
        }
    }

    return 0;
}

function ConfigurarDados($handle, $ffi, $AeArquivoIni, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_ConfigurarDados($handle->cdata, $AeArquivoIni);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao configurar dados", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function IncluirTitulos($handle, $ffi, $AeArquivoIni, $AeTpSaida, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_IncluirTitulos($handle->cdata, $AeArquivoIni, $AeTpSaida);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao incluir títulos", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GerarRemessaStream($handle, $ffi, $AeNumArquivo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_GerarRemessaStream($handle->cdata, $AeNumArquivo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gerar remessa stream", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SalvarPDF($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_SalvarPDF($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao salvar pdf stream", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GerarPDF($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_GerarPDF($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gerar pdf", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function LerRetornoStream($handle, $ffi, $AeArquivoIni, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_LerRetornoStream($handle->cdata, $AeArquivoIni, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao ler retorno stream", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEmail($handle, $ffi, $AePara, $AeAssunto, $AeMensagem, $AeCC, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_EnviarEmail($handle->cdata, $AePara, $AeAssunto, $AeMensagem, $AeCC);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar e-mail", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    if ($retornoGeral == "") {
        $retornoGeral = "E-mail Enviado";
    }

    return 0;
}

function EnviarEmailBoleto($handle, $ffi, $AeIndice, $AePara, $AeAssunto, $AeMensagem, $AeCC, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_EnviarEmailBoleto($handle->cdata, $AeIndice, $AePara, $AeAssunto, $AeMensagem, $AeCC);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar e-mail", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    if ($retornoGeral == "") {
        $retornoGeral = "E-mail Enviado";
    }

    return 0;
}

function RetornaLinhaDigitavel($handle, $ffi, $AeIndice, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_RetornaLinhaDigitavel($handle->cdata, $AeIndice, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao ler linha digitável", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function RetornaCodigoBarras($handle, $ffi, $AeIndice, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_RetornaCodigoBarras($handle->cdata, $AeIndice, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao ler código de barras", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SelecionaBanco($handle, $ffi, $AeCodBanco, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_SelecionaBanco($handle->cdata, $AeCodBanco);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao selecionar o banco", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ListaOcorrencias($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 16096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_ListaOcorrencias($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao listar ocorrências", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = str_replace('|', "\n", $retornoGeral);

    return 0;
}

function ListaOcorrenciasEX($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 16096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_ListaOcorrenciasEX($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao listar ocorrências EX", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = str_replace('|', "\n", $retornoGeral);

    return 0;
}

function ListaCaractTitulo($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_ListaCaractTitulo($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao listar carac. títulos", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = str_replace('|', "\n", $retornoGeral);

    return 0;
}

function CodigosMoraAceitos($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_CodigosMoraAceitos($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao listar códigos mora aceitos", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $digitos = str_split($retornoGeral);

    $retornoGeral = implode("\n", $digitos);

    return 0;
}

function TamNossoNumero($handle, $ffi, $AeCarteira, $AenossoNumero, $AeConvenio, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_TamNossoNumero($handle->cdata, $AeCarteira, $AenossoNumero, $AeConvenio);

    if ($retorno < 0)
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro verificar o tam. nosso número", 1) != 0)
            return -10;

    $retornoGeral = $retorno;

    return 0;
}

function MontarNossoNumero($handle, $ffi, $AeIndice, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_MontarNossoNumero($handle->cdata, $AeIndice, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao montar nosso número", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ListaBancos($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_ListaBancos($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao listar bancos", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = str_replace('|', "\n", $retornoGeral);

    return 0;
}

function EnviarBoleto($handle, $ffi, $AeCodigoOperacao, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Boleto_EnviarBoleto($handle->cdata, $AeCodigoOperacao, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar boleto API/WebService, operação[$AeCodigoOperacao]", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    if ($retornoGeral == "")
        $retornoGeral = "Ocorreu um erro na requisição";

    return 0;
}

function ConsultarTitulosPorPeriodo($handle, $ffi, $eArquivoIni, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Boleto_ConsultarTitulosPorPeriodo($handle->cdata, $eArquivoIni, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar títulos por período", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    if ($retornoGeral == "")
        $retornoGeral = "Ocorreu um erro na requisição";

    return 0;
}