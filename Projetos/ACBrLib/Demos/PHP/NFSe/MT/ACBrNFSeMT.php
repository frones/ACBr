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

function Inicializar(&$handle, $ffi, $iniPath)
{
    $retorno = $ffi->NFSE_Inicializar(FFI::addr($handle), $iniPath, "");

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao inicializar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function Finalizar($handle, $ffi)
{
    $retorno = $ffi->NFSE_Finalizar($handle->cdata);

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao finalizar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function ConfigLer($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->NFSE_ConfigLer($handle->cdata, $eArqConfig);
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
    $retorno = $ffi->NFSE_ConfigLerValor($handle->cdata, $eSessao, $eChave, $sResposta, FFI::addr($esTamanho));

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
    $retorno = $ffi->NFSE_ConfigGravarValor($handle->cdata, $eSessao, $eChave, $value);
    $sMensagem = FFI::new("char[535]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gravar valores [$value] na secao[$eSessao] e chave[$eChave]. ") != 0)
        return -10;

    return 0;
}

function ConfigGravar($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->NFSE_ConfigGravar($handle->cdata, $eArqConfig);
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
        $resposta = $ffi->NFSE_UltimoRetorno($handle->cdata, $sMensagem, FFI::addr($esTamanho));

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

function OpenSSLInfo($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_OpenSSLInfo($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao verificar a OpenSSLInfo", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarINI($handle, $ffi, $eArquivoOuIni, &$retornoGeral)
{
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_CarregarINI($handle->cdata, $eArquivoOuIni);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o Ini da NFSe", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarXML($handle, $ffi, $eArquivoOuIni, &$retornoGeral)
{
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_CarregarXML($handle->cdata, $eArquivoOuIni);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o Xml da NFSe", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Emitir($handle, $ffi, $ALote, $AModoEnvio, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_Emitir($handle->cdata, $ALote, $AModoEnvio, "0", $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao emitir NFSe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = mb_convert_encoding($retornoGeral, "UTF-8", "ISO-8859-1");

    return 0;
}

function ConsultarSituacao($handle, $ffi, $AProtocolo, $ANumLote, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarSituacao($handle->cdata, $AProtocolo, $ANumLote, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar situação do protocolo [$AProtocolo], lote [$ANumLote]", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarLoteRps($handle, $ffi, $AProtocolo, $ANumLote, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarLoteRps($handle->cdata, $AProtocolo, $ANumLote, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar lote de RPS do protocolo [$AProtocolo], lote [$ANumLote]", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSePorPeriodo($handle, $ffi, $ADataInicial, $ADataFinal, $APagina, $ANumeroLote, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSePorPeriodo($handle->cdata, $ADataInicial, $ADataFinal, $APagina, $ANumeroLote, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar NFSe por período", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSePorNumero($handle, $ffi, $ANumero, $APagina, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSePorNumero($handle->cdata, $ANumero, $APagina, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar NFSe [$ANumero]", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSePorRps($handle, $ffi, $ANumeroRps, $ASerie, $ATipo, $ACodigoVerificacao, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSePorRps($handle->cdata, $ANumeroRps, $ASerie, $ATipo, $ACodigoVerificacao, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar NFSe por RPS", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeGenerico($handle, $ffi, $AInfConsultaNFSe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeGenerico($handle->cdata, $AInfConsultaNFSe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao executar consulta genérica de NFSe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSePorFaixa($handle, $ffi, $ANumeroInicial, $ANumeroFinal, $APagina, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSePorFaixa($handle->cdata, $ANumeroInicial, $ANumeroFinal, $APagina, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar NFSe por faixa", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarLinkNFSe($handle, $ffi, $AInfConsultaLinkNFSe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarLinkNFSe($handle->cdata, $AInfConsultaLinkNFSe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar link da NFSe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEmail($handle, $ffi, $AePara, $AeXmlNFSe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_EnviarEmail($handle->cdata, $AePara, $AeXmlNFSe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar e-mail", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function LinkNFSe($handle, $ffi, $ANumeroNFSe, $ACodigoVerificacao, $AChaveAcesso, $AValorServico, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_LinkNFSe($handle->cdata, $ANumeroNFSe, $ACodigoVerificacao, $AChaveAcesso, $AValorServico, $sMensagem, FFI::addr($esTamanho));

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gerar Link", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GerarToken($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_GerarToken($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gerar o Token", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SalvarPDF($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_SalvarPDF($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao salvar o pdf", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SubstituirNFSe($handle, $ffi, $ANumeroNFSe, $ASerieNFSe, $ACodigoCancelamento, $AMotivoCancelamento, $ANumeroLote, $ACodigoVerificacao, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_SubstituirNFSe($handle->cdata, $ANumeroNFSe, $ASerieNFSe, $ACodigoCancelamento, $AMotivoCancelamento, $ANumeroLote, $ACodigoVerificacao, $sMensagem, FFI::addr($esTamanho));

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao substituir NFSe", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Cancelar($handle, $ffi, $AInfCancelamentoNFSe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_Cancelar($handle->cdata, $AInfCancelamentoNFSe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao cancelar NFSe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoPrestadoPorNumero($handle, $ffi, $ANumero, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoPrestadoPorNumero($handle->cdata, $ANumero, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar serviços prestados por Número", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoPrestadoPorTomador($handle, $ffi, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoPrestadoPorTomador($handle->cdata, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar serviços prestados por Tomador", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoPrestadoPorPeriodo($handle, $ffi, $ADataInicial, $ADataFinal, $APagina, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoPrestadoPorPeriodo($handle->cdata, $ADataInicial, $ADataFinal, $APagina, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar serviços prestados por Período", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoPrestadoPorIntermediario($handle, $ffi, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoPrestadoPorIntermediario($handle->cdata, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar serviços prestados por Intermediário", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoTomadoPorNumero($handle, $ffi, $ANumero, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoTomadoPorNumero($handle->cdata, $ANumero, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar serviços tomados por Número", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoTomadoPorPrestador($handle, $ffi, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoTomadoPorPrestador($handle->cdata, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar serviços tomados por Prestador", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoTomadoPorTomador($handle, $ffi, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoTomadoPorTomador($handle->cdata, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar serviços tomados por Tomador", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoTomadoPorPeriodo($handle, $ffi, $ADataInicial, $ADataFinal, $APagina, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoTomadoPorPeriodo($handle->cdata, $ADataInicial, $ADataFinal, $APagina, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar serviços tomados por Período", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSeServicoTomadoPorIntermediario($handle, $ffi, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSeServicoTomadoPorIntermediario($handle->cdata, $ACNPJ, $AInscMun, $APagina, $ADataInicial, $ADataFinal, $ATipoPeriodo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar serviços tomados por Intermediário", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEvento($handle, $ffi, $AInfEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_EnviarEvento($handle->cdata, $AInfEvento, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarDPSPorChave($handle, $ffi, $AChaveDPS, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarDPSPorChave($handle->cdata, $AChaveDPS, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar DPS por Chave", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarNFSePorChave($handle, $ffi, $AChaveNFSe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarNFSePorChave($handle->cdata, $AChaveNFSe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar NFSe por Chave", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarEvento($handle, $ffi, $AChave, $ATipoEvento, $ANumSeq, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarEvento($handle->cdata, $AChave, $ATipoEvento, $ANumSeq, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar Evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarDFe($handle, $ffi, $ANSU, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarDFe($handle->cdata, $ANSU, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar DFe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ObterDANFSE($handle, $ffi, $AChaveNFSe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
 
    $retorno = $ffi->NFSE_ObterDANFSE($handle->cdata, $AChaveNFSe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao obter DANFSE", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarParametros($handle, $ffi, $ATipoParametroMunicipio, $ACodigoServico, $ACompetencia, $ANumeroBeneficio, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 18096;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFSE_ConsultarParametros($handle->cdata, $ATipoParametroMunicipio, $ACodigoServico, $ACompetencia, $ANumeroBeneficio, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar parâmetros", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}
