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
*/
header('Content-Type: application/json; charset=UTF-8');

function Inicializar($ffi, $iniPath)
{
    $retorno = $ffi->NFSE_Inicializar($iniPath, "");

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao inicializar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function Finalizar($ffi)
{
    $retorno = $ffi->NFSE_Finalizar();

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao finalizar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function ConfigLer($ffi, $eArqConfig)
{
    $retorno = $ffi->NFSE_ConfigLer($eArqConfig);
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
    $retorno = $ffi->NFSE_ConfigLerValor($eSessao, $eChave, $sResposta, FFI::addr($esTamanho));

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
    $retorno = $ffi->NFSE_ConfigGravarValor($eSessao, $eChave, $value);
    $sMensagem = FFI::new("char[535]");

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao gravar valores [$value] na secao[$eSessao] e chave[$eChave]. ") != 0)
        return -10;

    return 0;
}

function ConfigGravar($ffi, $eArqConfig)
{
    $retorno = $ffi->NFSE_ConfigGravar($eArqConfig);
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
        $resposta = $ffi->NFSE_UltimoRetorno($sMensagem, FFI::addr($esTamanho));

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

function OpenSSLInfo($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_OpenSSLInfo($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao verificar a OpenSSLInfo", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarINI($ffi, $eArquivoOuIni, &$retornoGeral)
{
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_CarregarINI($eArquivoOuIni);

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao carregar o Ini da NFSe", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarXML($ffi, $eArquivoOuIni, &$retornoGeral)
{
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_CarregarXML($eArquivoOuIni);

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao carregar o Xml da NFSe", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Emitir($ffi, $ALote, $AModoEnvio, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_Emitir($ALote, $AModoEnvio, "0", $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao emitir NFSe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = mb_convert_encoding($retornoGeral, "UTF-8", "ISO-8859-1");

    return 0;
}

function ConsultarSituacao($ffi, $AProtocolo, $ANumLote, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarSituacao($AProtocolo, $ANumLote, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar situação do protocolo [$AProtocolo], lote [$ANumLote]", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarLoteRps($ffi, $AProtocolo, $ANumLote, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarLoteRps($AProtocolo, $ANumLote, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar lote de RPS do protocolo [$AProtocolo], lote [$ANumLote]", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSePorPeriodo($ffi, $ADataInicial, $ADataFinal, $APagina, $ANumeroLote, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSePorPeriodo($ADataInicial, $ADataFinal, $APagina, $ANumeroLote, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar NFSe por período", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSePorNumero($ffi, $ANumero, $APagina, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSePorNumero($ANumero, $APagina, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar NFSe [$ANumero]", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSePorRps($ffi, $ANumeroRps, $ASerie, $ATipo, $ACodigoVerificacao, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSePorRps($ANumeroRps, $ASerie, $ATipo, $ACodigoVerificacao, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar NFSe por RPS", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeGenerico($ffi, $AInfConsultaNFSe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeGenerico($AInfConsultaNFSe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao executar consulta genérica de NFSe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSePorFaixa($ffi, $ANumeroInicial, $ANumeroFinal, $APagina, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSePorFaixa($ANumeroInicial, $ANumeroFinal, $APagina, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar NFSe por faixa", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarLinkNFSe($ffi, $AInfConsultaLinkNFSe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarLinkNFSe($AInfConsultaLinkNFSe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar link da NFSe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEmail($ffi, $AePara, $AeXmlNFSe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_EnviarEmail($AePara, $AeXmlNFSe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem);

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao enviar e-mail", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function LinkNFSe($ffi, $ANumeroNFSe, $ACodigoVerificacao, $AChaveAcesso, $AValorServico, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_LinkNFSe($ANumeroNFSe, $ACodigoVerificacao, $AChaveAcesso, $AValorServico, $sMensagem, FFI::addr($esTamanho));

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao gerar Link", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GerarToken($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_GerarToken($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao gerar o Token", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SalvarPDF($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_SalvarPDF($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao salvar o pdf", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SubstituirNFSe($ffi, $ANumeroNFSe, $ASerieNFSe, $ACodigoCancelamento, $AMotivoCancelamento, $ANumeroLote, $ACodigoVerificacao, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_SubstituirNFSe($ANumeroNFSe, $ASerieNFSe, $ACodigoCancelamento, $AMotivoCancelamento, $ANumeroLote, $ACodigoVerificacao, $sMensagem, FFI::addr($esTamanho));

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao substituir NFSe", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Cancelar($ffi, $AInfCancelamentoNFSe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_Cancelar($AInfCancelamentoNFSe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao cancelar NFSe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoPrestadoPorNumero($ffi, $ANumero, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoPrestadoPorNumero($ANumero, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar serviços prestados por Número", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoPrestadoPorTomador($ffi, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoPrestadoPorTomador($ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar serviços prestados por Tomador", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoPrestadoPorPeriodo($ffi, $ADataInicial, $ADataFinal, $APagina, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoPrestadoPorPeriodo($ADataInicial, $ADataFinal, $APagina, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar serviços prestados por Período", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoPrestadoPorIntermediario($ffi, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoPrestadoPorIntermediario($ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar serviços prestados por Intermediário", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoTomadoPorNumero($ffi, $ANumero, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoTomadoPorNumero($ANumero, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar serviços tomados por Número", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoTomadoPorPrestador($ffi, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoTomadoPorPrestador($ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar serviços tomados por Prestador", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoTomadoPorTomador($ffi, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoTomadoPorTomador($ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar serviços tomados por Tomador", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoTomadoPorPeriodo($ffi, $ADataInicial, $ADataFinal, $APagina, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoTomadoPorPeriodo($ADataInicial, $ADataFinal, $APagina, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar serviços tomados por Período", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoTomadoPorIntermediario($ffi, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoTomadoPorIntermediario($ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar serviços tomados por Intermediário", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEvento($ffi, $AInfEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_EnviarEvento($AInfEvento, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao enviar evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarDPSPorChave($ffi, $AChaveDPS, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarDPSPorChave($AChaveDPS, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar DPS por Chave", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSePorChave($ffi, $AChaveNFSe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSePorChave($AChaveNFSe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar NFSe por Chave", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarEvento($ffi, $AChave, $ATipoEvento, $ANumSeq, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarEvento($AChave, $ATipoEvento, $ANumSeq, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar Evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarDFe($ffi, $ANSU, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarDFe($ANSU, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar DFe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ObterDANFSE($ffi, $AChaveNFSe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
 
    $retorno = $ffi->NFSE_ObterDANFSE($AChaveNFSe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao obter DANFSE", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarParametros($ffi, $ATipoParametroMunicipio, $ACodigoServico, $ACompetencia, $ANumeroBeneficio, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarParametros($ATipoParametroMunicipio, $ACodigoServico, $ACompetencia, $ANumeroBeneficio, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar parâmetros", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}
