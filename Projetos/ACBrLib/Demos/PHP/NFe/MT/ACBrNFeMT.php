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
    $retorno = $ffi->NFE_Inicializar(FFI::addr($handle), $iniPath, "");

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao inicializar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function Finalizar($handle, $ffi)
{
    $retorno = $ffi->NFE_Finalizar($handle->cdata);

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao finalizar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function ConfigLer($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->NFE_ConfigLer($handle->cdata, $eArqConfig);
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
    $retorno = $ffi->NFE_ConfigLerValor($handle->cdata, $eSessao, $eChave, $sResposta, FFI::addr($esTamanho));

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
    $retorno = $ffi->NFE_ConfigGravarValor($handle->cdata, $eSessao, $eChave, $value);
    $sMensagem = FFI::new("char[535]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gravar valores [$value] na secao[$eSessao] e chave[$eChave]. ") != 0)
        return -10;

    return 0;
}

function ConfigGravar($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->NFE_ConfigGravar($handle->cdata, $eArqConfig);
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
        $resposta = $ffi->NFE_UltimoRetorno($handle->cdata, $sMensagem, FFI::addr($esTamanho));

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

function StatusServico($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_StatusServico($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar status do serviço", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function NFENome($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_Nome($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao verificar o nome da biblioteca", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function NFEVersao($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_Versao($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao verificar a versão da biblioteca", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function OpenSSLInfo($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_OpenSSLInfo($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao verificar a OpenSSLInfo", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarXmlNfe($handle, $ffi, $eArquivoOuXML, &$retornoGeral)
{
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_CarregarXML($handle->cdata, $eArquivoOuXML);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o XML da NFe", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarINI($handle, $ffi, $eArquivoOuINI, &$retornoGeral)
{
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_CarregarINI($handle->cdata, $eArquivoOuINI);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o INI", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarEventoXML($handle, $ffi, $eArquivoOuXML, &$retornoGeral)
{
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_CarregarEventoXML($handle->cdata, $eArquivoOuXML);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o XML do evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function LimparListaNfe($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_LimparLista($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao limpar a lista de NFes.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function LimparListaEventos($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_LimparListaEventos($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao limpar a lista de eventos.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function AssinarNFe($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_Assinar($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro assinar NFe.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ValidarNFe($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_Validar($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro validar NFe.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ValidarRegrasdeNegocios($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ValidarRegrasdeNegocios($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao validar regra de negócio", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function VerificarAssinatura($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_VerificarAssinatura($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao verificar assinatura", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ObterXml($handle, $ffi, $AIndex, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ObterXml($handle->cdata, $AIndex, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao obter o xml", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GravarXml($handle, $ffi, $AIndex, $eNomeArquivo, $ePathArquivo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_GravarXml($handle->cdata, $AIndex, $eNomeArquivo, $ePathArquivo);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro gravar o xml da NFe.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ObterIni($handle, $ffi, $AIndex, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ObterIni($handle->cdata, $AIndex, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao obter o ini", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GravarIni($handle, $ffi, $AIndex, $eNomeArquivo, $ePathArquivo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_GravarIni($handle->cdata, $AIndex, $eNomeArquivo, $ePathArquivo);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro gravar o ini", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarEventoINI($handle, $ffi, $eArquivoOuINI, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_CarregarEventoINI($handle->cdata, $eArquivoOuINI);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro carregar o ini do evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GerarChave(
    $handle,
    $ffi,
    $ACodigoUF,
    $ACodigoNumerico,
    $AModelo,
    $ASerie,
    $ANumero,
    $ATpEmi,
    $AEmissao,
    $ACNPJCPF,
    &$retornoGeral
) {
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_GerarChave(
        $handle->cdata,
        $ACodigoUF,
        $ACodigoNumerico,
        $AModelo,
        $ASerie,
        $ANumero,
        $ATpEmi,
        $AEmissao,
        $ACNPJCPF,
        $sMensagem,
        FFI::addr($esTamanho)
    );

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao obter o ini", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ObterCertificados($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ObterCertificados($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao obter certificados", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GetPath($handle, $ffi, $ATipo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_GetPath($handle->cdata, $ATipo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao obter o caminnho", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function GetPathEvento($handle, $ffi, $ACodEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_GetPathEvento($handle->cdata, $ACodEvento, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao obter o caminnho do evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Consultar($handle, $ffi, $eChaveOuNFe, $AExtrairEventos, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_Consultar($handle->cdata, $eChaveOuNFe, $AExtrairEventos, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar a chave", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Inutilizar($handle, $ffi, $ACNPJ, $AJustificativa, $Ano, $Modelo, $Serie, $NumeroInicial, $NumeroFinal, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_Inutilizar($handle->cdata, $ACNPJ, $AJustificativa, $Ano, $Modelo, $Serie, $NumeroInicial, $NumeroFinal, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao inutilizar faixa", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Enviar($handle, $ffi, $ALote, $AImprimir, $ASincrono, $AZipado, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_Enviar($handle->cdata, $ALote, $AImprimir, $ASincrono, $AZipado, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar NFe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarRecibo($handle, $ffi, $ARecibo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ConsultarRecibo($handle->cdata, $ARecibo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar recibo", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Cancelar($handle, $ffi, $AeChave, $AeJustificativa, $AeCNPJCPF, $ALote, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_Cancelar($handle->cdata, $AeChave, $AeJustificativa, $AeCNPJCPF, $ALote, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao cancelar NFe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEvento($handle, $ffi, $AidLote, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_EnviarEvento($handle->cdata, $AidLote, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar o evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultaCadastro($handle, $ffi, $AcUF, $AnDocumento, $AnIE, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ConsultaCadastro($handle->cdata, $AcUF, $AnDocumento, $AnIE, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar cadastro", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function DistribuicaoDFePorUltNSU($handle, $ffi, $AcUFAutor, $AeCNPJCPF, $AeultNSU, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_DistribuicaoDFePorUltNSU($handle->cdata, $AcUFAutor, $AeCNPJCPF, $AeultNSU, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao executar Distribuição por UltNSU", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function DistribuicaoDFe($handle, $ffi, $AcUFAutor, $AeCNPJCPF, $AeultNSU, $AeArquivoOuXML, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_DistribuicaoDFe($handle->cdata, $AcUFAutor, $AeCNPJCPF, $AeultNSU, $AeArquivoOuXML, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao executar Distribuição DFe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function DistribuicaoDFePorNSU($handle, $ffi, $AcUFAutor, $AeCNPJCPF, $AeNSU, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_DistribuicaoDFePorNSU($handle->cdata, $AcUFAutor, $AeCNPJCPF, $AeNSU, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao executar Distribuição por NSU", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function DistribuicaoDFePorChave($handle, $ffi, $AcUFAutor, $AeCNPJCPF, $AechNFe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_DistribuicaoDFePorChave($handle->cdata, $AcUFAutor, $AeCNPJCPF, $AechNFe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao executar Distribuição por chave", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEmail($handle, $ffi, $AePara, $AeChaveNFe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_EnviarEmail($handle->cdata, $AePara, $AeChaveNFe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar e-mail", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEmailEvento($handle, $ffi, $AePara, $AeChaveEvento, $AeChaveNFe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_EnviarEmailEvento($handle->cdata, $AePara, $AeChaveEvento, $AeChaveNFe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar e-mail do evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Imprimir($handle, $ffi, $AcImpressora, $AnNumCopias, $AcProtocolo, $AbMostrarPreview, $AcMarcaDagua, $AbViaConsumidor, $AbSimplificado, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_Imprimir($handle->cdata, $AcImpressora, $AnNumCopias, $AcProtocolo, $AbMostrarPreview, $AcMarcaDagua, $AbViaConsumidor, $AbSimplificado);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirPDF($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ImprimirPDF($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir o pdf", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SalvarPDF($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_SalvarPDF($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao salvar o pdf", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirEvento($handle, $ffi, $AeArquivoXmlNFe, $AeArquivoXmlEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ImprimirEvento($handle->cdata, $AeArquivoXmlNFe, $AeArquivoXmlEvento);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir o evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirEventoPDF($handle, $ffi, $AeArquivoXmlNFe, $AeArquivoXmlEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ImprimirEventoPDF($handle->cdata, $AeArquivoXmlNFe, $AeArquivoXmlEvento);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir o pdf do evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SalvarEventoPDF($handle, $ffi, $AeArquivoXmlNFe, $AeArquivoXmlEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_SalvarEventoPDF($handle->cdata, $AeArquivoXmlNFe, $AeArquivoXmlEvento, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao salvar o pdf do evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirInutilizacao($handle, $ffi, $AeArquivoXml, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ImprimirInutilizacao($handle->cdata, $AeArquivoXml);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir evento de inutilização", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirInutilizacaoPDF($handle, $ffi, $AeArquivoXml, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_ImprimirInutilizacaoPDF($handle->cdata, $AeArquivoXml);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir o pdf de inutilização", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SalvarInutilizacaoPDF($handle, $ffi, $AeArquivoXml, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[535]");
    $retorno = $ffi->NFE_SalvarInutilizacaoPDF($handle->cdata, $AeArquivoXml, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao salvar o pdf de inutilização", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}