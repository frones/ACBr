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
    $retorno = $ffi->CTE_Inicializar(FFI::addr($handle), $iniPath, "");

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao inicializar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function Finalizar($handle, $ffi)
{
    $retorno = $ffi->CTE_Finalizar($handle->cdata);

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao finalizar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function ConfigLer($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->CTE_ConfigLer($handle->cdata, $eArqConfig);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o arquivo ini. ") != 0)
        return -10;

    return 0;
}

function ConfigLerValor($handle, $ffi, $eSessao, $eChave, &$sValor)
{
    $sResposta = FFI::new("char[9048]");
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $retorno = $ffi->CTE_ConfigLerValor($handle->cdata, $eSessao, $eChave, $sResposta, FFI::addr($esTamanho));

    $sMensagem = FFI::new("char[9048]");

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao ler valor na secao[$eSessao] e chave[$eChave]. ", 1) != 0)
            return -10;
    }

    $sValor = FFI::string($sResposta);
    return 0;
}

function ConfigGravarValor($handle, $ffi, $eSessao, $eChave, $value)
{
    $retorno = $ffi->CTE_ConfigGravarValor($handle->cdata, $eSessao, $eChave, $value);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gravar valores [$value] na secao[$eSessao] e chave[$eChave]. ") != 0)
        return -10;

    return 0;
}

function ConfigGravar($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->CTE_ConfigGravar($handle->cdata, $eArqConfig);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gravar as configurações. ") != 0)
        return -10;

    return 0;
}

function UltimoRetorno($handle, $ffi, $retornolib, &$sMensagem, $msgErro, $retMensagem = 0)
{
    if (($retornolib !== 0) || ($retMensagem == 1)) {
        $esTamanho = FFI::new("long");
        $esTamanho->cdata = 9048;
        $resposta = $ffi->CTE_UltimoRetorno($handle->cdata, $sMensagem, FFI::addr($esTamanho));

        if ($retornolib !== 0) {
            $ultimoRetorno = FFI::string($sMensagem);
            $retorno = "$msgErro Código de erro: $retornolib. ";

            if ($ultimoRetorno != "") {
                $retorno = $retorno . "Último retorno: " . $ultimoRetorno;
            }

            $retorno = mb_convert_encoding($retorno, "UTF-8", "ISO-8859-1");

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_StatusServico($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar status do serviço", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CTENome($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_Nome($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao verificar o nome da biblioteca", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CTEVersao($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_Versao($handle->cdata, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_OpenSSLInfo($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao verificar a OpenSSLInfo", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarXmlCTe($handle, $ffi, $eArquivoOuXML, &$retornoGeral)
{
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_CarregarXML($handle->cdata, $eArquivoOuXML);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o XML da CTe", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarINI($handle, $ffi, $eArquivoOuINI, &$retornoGeral)
{
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_CarregarINI($handle->cdata, $eArquivoOuINI);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o INI", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarEventoXML($handle, $ffi, $eArquivoOuXML, &$retornoGeral)
{
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_CarregarEventoXML($handle->cdata, $eArquivoOuXML);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o XML do evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function LimparListaCTe($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_LimparLista($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao limpar a lista de CTes.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function LimparListaEventos($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_LimparListaEventos($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao limpar a lista de eventos.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function AssinarCTe($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_Assinar($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro assinar CTe.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ValidarCTe($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_Validar($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro validar CTe.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ValidarRegrasdeNegocios($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_ValidarRegrasdeNegocios($handle->cdata, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_VerificarAssinatura($handle->cdata, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_ObterXml($handle->cdata, $AIndex, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_GravarXml($handle->cdata, $AIndex, $eNomeArquivo, $ePathArquivo);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro gravar o xml da CTe.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ObterIni($handle, $ffi, $AIndex, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_ObterIni($handle->cdata, $AIndex, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_GravarIni($handle->cdata, $AIndex, $eNomeArquivo, $ePathArquivo);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro gravar o ini", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarEventoINI($handle, $ffi, $eArquivoOuINI, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_CarregarEventoINI($handle->cdata, $eArquivoOuINI);

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_GerarChave(
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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_ObterCertificados($handle->cdata, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_GetPath($handle->cdata, $ATipo, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_GetPathEvento($handle->cdata, $ACodEvento, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao obter o caminnho do evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Consultar($handle, $ffi, $eChaveOuCTe, $AExtrairEventos, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
    $retorno = $ffi->CTE_Consultar($handle->cdata, $eChaveOuCTe, $AExtrairEventos, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar a chave", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Enviar($handle, $ffi, $ALote, $AImprimir, $ASincrono, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_Enviar($handle->cdata, $ALote, $AImprimir, $ASincrono, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar CTe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarRecibo($handle, $ffi, $ARecibo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_ConsultarRecibo($handle->cdata, $ARecibo, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_Cancelar($handle->cdata, $AeChave, $AeJustificativa, $AeCNPJCPF, $ALote, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao cancelar CTe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEvento($handle, $ffi, $AidLote, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_EnviarEvento($handle->cdata, $AidLote, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_ConsultaCadastro($handle->cdata, $AcUF, $AnDocumento, $AnIE, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_DistribuicaoDFePorUltNSU($handle->cdata, $AcUFAutor, $AeCNPJCPF, $AeultNSU, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_DistribuicaoDFe($handle->cdata, $AcUFAutor, $AeCNPJCPF, $AeultNSU, $AeArquivoOuXML, $sMensagem, FFI::addr($esTamanho));

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_DistribuicaoDFePorNSU($handle->cdata, $AcUFAutor, $AeCNPJCPF, $AeNSU, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao executar Distribuição por NSU", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function DistribuicaoDFePorChave($handle, $ffi, $AcUFAutor, $AeCNPJCPF, $AechCTe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_DistribuicaoDFePorChave($handle->cdata, $AcUFAutor, $AeCNPJCPF, $AechCTe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao executar Distribuição por chave", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEmail($handle, $ffi, $AePara, $AeChaveCTe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_EnviarEmail($handle->cdata, $AePara, $AeChaveCTe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar e-mail", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEmailEvento($handle, $ffi, $AePara, $AeChaveEvento, $AeChaveCTe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_EnviarEmailEvento($handle->cdata, $AePara, $AeChaveEvento, $AeChaveCTe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar e-mail do evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Imprimir($handle, $ffi, $AcImpressora, $AnNumCopias, $AcProtocolo, $AbMostrarPreview, $AcMarcaDagua, $AbViaConsumidor, $AbSimplificado, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_Imprimir($handle->cdata, $AcImpressora, $AnNumCopias, $AcProtocolo, $AbMostrarPreview, $AcMarcaDagua, $AbViaConsumidor, $AbSimplificado);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirPDF($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_ImprimirPDF($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir o pdf", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SalvarPDF($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
    $retorno = $ffi->CTE_SalvarPDF($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao salvar o pdf", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirEvento($handle, $ffi, $AeArquivoXmlCTe, $AeArquivoXmlEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_ImprimirEvento($handle->cdata, $AeArquivoXmlCTe, $AeArquivoXmlEvento);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir o evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirEventoPDF($handle, $ffi, $AeArquivoXmlCTe, $AeArquivoXmlEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_ImprimirEventoPDF($handle->cdata, $AeArquivoXmlCTe, $AeArquivoXmlEvento);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir o pdf do evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SalvarEventoPDF($handle, $ffi, $AeArquivoXmlCTe, $AeArquivoXmlEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
    $retorno = $ffi->CTE_SalvarEventoPDF($handle->cdata, $AeArquivoXmlCTe, $AeArquivoXmlEvento, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao salvar o pdf do evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Inutilizar($handle, $ffi, $ACNPJ, $AJustificativa, $Ano, $Modelo, $Serie, $NumeroInicial, $NumeroFinal, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->CTE_Inutilizar($handle->cdata, $ACNPJ, $AJustificativa, $Ano, $Modelo, $Serie, $NumeroInicial, $NumeroFinal, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao inutilizar faixa", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirInutilizacao($handle, $ffi, $AeArquivoXml, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
    $retorno = $ffi->CTE_ImprimirInutilizacao($handle->cdata, $AeArquivoXml);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir evento de inutilização", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirInutilizacaoPDF($handle, $ffi, $AeArquivoXml, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
    $retorno = $ffi->CTE_ImprimirInutilizacaoPDF($handle->cdata, $AeArquivoXml);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir o pdf de inutilização", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}
