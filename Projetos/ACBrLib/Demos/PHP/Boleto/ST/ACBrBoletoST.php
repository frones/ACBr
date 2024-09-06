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

function Inicializar($ffi, $iniPath)
{
    $retorno = $ffi->Boleto_Inicializar($iniPath, "");

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao inicializar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function Finalizar($ffi)
{
    $retorno = $ffi->Boleto_Finalizar();

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao finalizar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function ConfigLer($ffi, $eArqConfig)
{
    $retorno = $ffi->Boleto_ConfigLer($eArqConfig);
    $sMensagem = FFI::new("char[535]");

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao carregar o arquivo ini. ") != 0)
        return -10;

    return 0;
}

function ConfigLerValor($ffi, $eSessao, $eChave, &$sValor)
{
    $sResposta = FFI::new("char[9048]");
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $retorno = $ffi->Boleto_ConfigLerValor($eSessao, $eChave, $sResposta, FFI::addr($esTamanho));

    $sMensagem = FFI::new("char[535]");

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao ler valor na secao[$eSessao] e chave[$eChave]. ", 1) != 0)
            return -10;
    }

    $sValor = FFI::string($sResposta);
    return 0;
}

function ConfigGravarValor($ffi, $eSessao, $eChave, $value)
{
    $retorno = $ffi->Boleto_ConfigGravarValor($eSessao, $eChave, $value);
    $sMensagem = FFI::new("char[535]");

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao gravar valores [$value] na secao[$eSessao] e chave[$eChave]. ") != 0)
        return -10;

    return 0;
}

function ConfigGravar($ffi, $eArqConfig)
{
    $retorno = $ffi->Boleto_ConfigGravar($eArqConfig);
    $sMensagem = FFI::new("char[535]");

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao gravar as configurações. ") != 0)
        return -10;

    return 0;
}

function UltimoRetorno($ffi, $retornolib, &$sMensagem, $msgErro, $retMensagem = 0)
{
    if (($retornolib !== 0) || ($retMensagem == 1)) {
        $esTamanho = FFI::new("long");
        $esTamanho->cdata = 9048;
        $ffi->Boleto_UltimoRetorno($sMensagem, FFI::addr($esTamanho));

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

function ConfigurarDados($ffi, $AeArquivoIni, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_ConfigurarDados($AeArquivoIni);

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao configurar dados", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function IncluirTitulos($ffi, $AeArquivoIni, $AeTpSaida, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_IncluirTitulos($AeArquivoIni, $AeTpSaida);

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao incluir títulos", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GerarRemessaStream($ffi, $AeNumArquivo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_GerarRemessaStream($AeNumArquivo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao gerar remessa stream", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SalvarPDF($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_SalvarPDF($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao salvar pdf stream", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GerarPDF($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_GerarPDF();

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao gerar pdf", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function LerRetornoStream($ffi, $AeArquivoIni, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_LerRetornoStream($AeArquivoIni, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao ler retorno stream", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEmail($ffi, $AePara, $AeAssunto, $AeMensagem, $AeCC, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_EnviarEmail($AePara, $AeAssunto, $AeMensagem, $AeCC);

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao enviar e-mail", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    if ($retornoGeral == "") {
        $retornoGeral = "E-mail Enviado";
    }

    return 0;
}

function EnviarEmailBoleto($ffi, $AeIndice, $AePara, $AeAssunto, $AeMensagem, $AeCC, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_EnviarEmailBoleto($AeIndice, $AePara, $AeAssunto, $AeMensagem, $AeCC);

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao enviar e-mail", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    if ($retornoGeral == "") {
        $retornoGeral = "E-mail Enviado";
    }

    return 0;
}

function RetornaLinhaDigitavel($ffi, $AeIndice, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_RetornaLinhaDigitavel($AeIndice, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao ler linha digitável", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function RetornaCodigoBarras($ffi, $AeIndice, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_RetornaCodigoBarras($AeIndice, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao ler código de barras", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SelecionaBanco($ffi, $AeCodBanco, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_SelecionaBanco($AeCodBanco);

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao selecionar o banco", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ListaOcorrencias($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 16096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_ListaOcorrencias($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao listar ocorrências", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = str_replace('|', "\n", $retornoGeral);

    return 0;
}

function ListaOcorrenciasEX($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 16096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_ListaOcorrenciasEX($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao listar ocorrências EX", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = str_replace('|', "\n", $retornoGeral);

    return 0;
}

function ListaCaractTitulo($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_ListaCaractTitulo($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao listar carac. títulos", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = str_replace('|', "\n", $retornoGeral);

    return 0;
}

function CodigosMoraAceitos($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_CodigosMoraAceitos($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao listar códigos mora aceitos", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $digitos = str_split($retornoGeral);

    $retornoGeral = implode("\n", $digitos);

    return 0;
}

function TamNossoNumero($ffi, $AeCarteira, $AenossoNumero, $AeConvenio, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_TamNossoNumero($AeCarteira, $AenossoNumero, $AeConvenio);

    if ($retorno < 0)
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro verificar o tam. nosso número", 1) != 0)
            return -10;

    $retornoGeral = $retorno;

    return 0;
}

function MontarNossoNumero($ffi, $AeIndice, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_MontarNossoNumero($AeIndice, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao montar nosso número", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ListaBancos($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->Boleto_ListaBancos($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao listar bancos", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = str_replace('|', "\n", $retornoGeral);

    return 0;
}

function EnviarBoleto($ffi, $AeCodigoOperacao, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Boleto_EnviarBoleto($AeCodigoOperacao, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao enviar boleto API/WebService, operação[$AeCodigoOperacao]", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    if ($retornoGeral == "")
        $retornoGeral = "Ocorreu um erro na requisição";

    return 0;
}

function ConsultarTitulosPorPeriodo($ffi, $eArquivoIni, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Boleto_ConsultarTitulosPorPeriodo($eArquivoIni, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar títulos por período", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    if ($retornoGeral == "")
        $retornoGeral = "Ocorreu um erro na requisição";

    return 0;
}