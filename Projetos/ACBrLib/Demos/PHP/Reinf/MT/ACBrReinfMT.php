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
    $retorno = $ffi->Reinf_Inicializar(FFI::addr($handle), $iniPath, "");

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao inicializar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function Finalizar($handle, $ffi)
{
    $retorno = $ffi->Reinf_Finalizar($handle->cdata);

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao finalizar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function ConfigLer($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->Reinf_ConfigLer($handle->cdata, $eArqConfig);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o arquivo ini.") != 0)
        return -10;

    return 0;
}

function ConfigLerValor($handle, $ffi, $eSessao, $eChave, &$sValor)
{
    $sResposta = FFI::new("char[9048]");
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $retorno = $ffi->Reinf_ConfigLerValor($handle->cdata, $eSessao, $eChave, $sResposta, FFI::addr($esTamanho));

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
    $retorno = $ffi->Reinf_ConfigGravarValor($handle->cdata, $eSessao, $eChave, $value);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gravar valores [$value] na secao[$eSessao] e chave[$eChave].") != 0)
        return -10;

    return 0;
}

function ConfigGravar($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->Reinf_ConfigGravar($handle->cdata, $eArqConfig);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gravar as configurações.") != 0)
        return -10;

    return 0;
}

function UltimoRetorno($handle, $ffi, $retornolib, &$sMensagem, $msgErro, $retMensagem = 0)
{
    if (($retornolib !== 0) || ($retMensagem == 1)) {
        $esTamanho = FFI::new("long");
        $esTamanho->cdata = 9048;
        $resposta = $ffi->Reinf_UltimoRetorno($handle->cdata, $sMensagem, FFI::addr($esTamanho));

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

function ReinfNome($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Reinf_Nome($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao verificar o nome da biblioteca", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ReinfVersao($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Reinf_Versao($handle->cdata, $sMensagem, FFI::addr($esTamanho));

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
    $retorno = $ffi->Reinf_OpenSSLInfo($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao verificar a OpenSSLInfo", 1) != 0)
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
    $retorno = $ffi->Reinf_ObterCertificados($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao obter certificados", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConfigImportar($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->Reinf_ConfigImportar($handle->cdata, $eArqConfig);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao importar as configurações.") != 0)
        return -10;

    return 0;
}

function ConfigExportar($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Reinf_ConfigExportar($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao exportar as configurações", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CriarEventoReinf($handle, $ffi, $eArqIni)
{
    $retorno = $ffi->Reinf_CriarEventoReinf($handle->cdata, $eArqIni);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao criar o evento.") != 0)
        return -10;

    return 0;
}

function LimparReinf($handle, $ffi, &$retornoGeral)
{
    $retorno = $ffi->Reinf_LimparReinf($handle->cdata);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao limpar eventos.") != 0)
        return -10;

    $retornoGeral = "ok";

    return 0;
}

function SetIDContribuinte($handle, $ffi, $aIdContribuinte, &$retornoGeral)
{
    $retorno = $ffi->Reinf_SetIDContribuinte($handle->cdata, $aIdContribuinte);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao setar ID do Contribuinte.") != 0)
        return -10;

    $retornoGeral = "ok";

    return 0;
}

function SetIDTransmissor($handle, $ffi, $aIdContribuinte, &$retornoGeral)
{
    $retorno = $ffi->Reinf_SetIDTransmissor($handle->cdata, $aIdContribuinte);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao setar ID do Transmissor.") != 0)
        return -10;

    $retornoGeral = "ok";

    return 0;
}

function SetTipoContribuinte($handle, $ffi, $aTipoContribuinte, &$retornoGeral)
{
    $retorno = $ffi->Reinf_SetTipoContribuinte($handle->cdata, $aTipoContribuinte);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao setar o Tipo de Contribuinte.") != 0)
        return -10;

    $retornoGeral = "ok";

    return 0;
}

function SetVersaoDF($handle, $ffi, $aIdContribuinte, &$retornoGeral)
{
    $retorno = $ffi->Reinf_SetVersaoDF($handle->cdata, $aIdContribuinte);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao setar a versão do Reinf.") != 0)
        return -10;

    $retornoGeral = "ok";

    return 0;
}

function Validar($handle, $ffi, &$retornoGeral)
{
    $retorno = $ffi->Reinf_Validar($handle->cdata);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao validar eventos.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    $retornoGeral = str_replace("Carregada", "Validado", $retornoGeral);

    return 0;
}

function CriarEnviarReinf($handle, $ffi, $eArqIni, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Reinf_CriarEnviarReinf($handle->cdata, $eArqIni, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao Criar e Enviar Evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarProtocolo($handle, $ffi, $eProtocolo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Reinf_ConsultarReinf($handle->cdata, $eProtocolo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar o protocolo", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultarReciboReinf($handle, $ffi, $ePerApur, $aTipoEvento, $eNrInscEstab, $eCnpjPrestador, $eNrInscTomador, $eDtApur, $eCpfCnpjBenef, $eCnpjFonte, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Reinf_ConsultarReciboReinf($handle->cdata, $ePerApur, $aTipoEvento, $eNrInscEstab, $eCnpjPrestador, $eNrInscTomador, $eDtApur, $eCpfCnpjBenef, $eCnpjFonte, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar o recibo", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarXMLEventoReinf($handle, $ffi, $eArquivoOuXML, &$retornoGeral)
{
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Reinf_CarregarXMLEventoReinf($handle->cdata, $eArquivoOuXML);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o XML do Evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarReinf($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->Reinf_EnviarReinf($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}
