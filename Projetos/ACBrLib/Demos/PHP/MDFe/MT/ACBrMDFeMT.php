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
    $retorno = $ffi->MDFE_Inicializar(FFI::addr($handle), $iniPath, "");

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao inicializar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function Finalizar($handle, $ffi)
{
    $retorno = $ffi->MDFE_Finalizar($handle->cdata);

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao finalizar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function ConfigLer($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->MDFE_ConfigLer($handle->cdata, $eArqConfig);
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
    $retorno = $ffi->MDFE_ConfigLerValor($handle->cdata, $eSessao, $eChave, $sResposta, FFI::addr($esTamanho));

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
    $retorno = $ffi->MDFE_ConfigGravarValor($handle->cdata, $eSessao, $eChave, $value);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gravar valores [$value] na secao[$eSessao] e chave[$eChave]. ") != 0)
        return -10;

    return 0;
}

function ConfigGravar($handle, $ffi, $eArqConfig)
{
    $retorno = $ffi->MDFE_ConfigGravar($handle->cdata, $eArqConfig);
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
        $resposta = $ffi->MDFE_UltimoRetorno($handle->cdata, $sMensagem, FFI::addr($esTamanho));

        if ($retornolib !== 0) {
            $ultimoRetorno = mb_convert_encoding(FFI::string($sMensagem), "UTF-8", "ISO-8859-1");
            $retorno = "$msgErro Código de erro: $retornolib. ";

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
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_StatusServico($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar status do serviço", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function MDFENome($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_Nome($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao verificar o nome da biblioteca", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function MDFEVersao($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_Versao($handle->cdata, $sMensagem, FFI::addr($esTamanho));

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
    $retorno = $ffi->MDFE_OpenSSLInfo($handle->cdata, $sMensagem, FFI::addr($esTamanho));

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
    $retorno = $ffi->MDFE_ObterCertificados($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao Obter Certificados", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarXmlMDFe($handle, $ffi, $eArquivoOuXML, &$retornoGeral)
{
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_CarregarXML($handle->cdata, $eArquivoOuXML);
    $respostaFunc = 0;

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o XML do MDFe", 1) != 0)
        $respostaFunc = -10;

    $retornoGeral = FFI::string($sMensagem);

    return $respostaFunc;
}

function CarregarINI($handle, $ffi, $eArquivoOuINI, &$retornoGeral)
{
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_CarregarINI($handle->cdata, $eArquivoOuINI);
    $respostaFunc = 0;

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao carregar o INI", 1) != 0)
        $respostaFunc = -10;

    $retornoGeral = FFI::string($sMensagem);

    return $respostaFunc;
}

function Enviar($handle, $ffi, $ALote, $AImprimir, $ASincrono, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_Enviar($handle->cdata, $ALote, $AImprimir, $ASincrono, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar MDFe", 1) != 0)
            return -10;
    }

    $retornoGeral = mb_convert_encoding(FFI::string($sMensagem), "UTF-8", "ISO-8859-1");

    return 0;
}

function AssinarMDFe($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_Assinar($handle->cdata);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro assinar MDFe.", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirPDF($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_ImprimirPDF($handle->cdata);

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
    $retorno = $ffi->MDFE_SalvarPDF($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao salvar o pdf", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ValidarRegrasdeNegocios($handle, $ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_ValidarRegrasdeNegocios($handle->cdata, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao validar regra de negócio", 1) != 0)
            return -10;
    }

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
    $esTamanho->cdata = 1024;
    $sMensagem = FFI::new("char[1024]");
    $retorno = $ffi->MDFE_GerarChave(
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
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao gerar a chave", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEmail($handle, $ffi, $AePara, $eArquivoXmlMDFe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_EnviarEmail($handle->cdata, $AePara, $eArquivoXmlMDFe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar e-mail", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function Consultar($handle, $ffi, $eChaveOuMDFe, $AExtrairEventos, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_Consultar($handle->cdata, $eChaveOuMDFe, $AExtrairEventos, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar a chave", 1) != 0)
            return -10;
    }

    $retornoGeral = mb_convert_encoding(FFI::string($sMensagem), "UTF-8", "ISO-8859-1");

    return 0;
}

function ConsultarRecibo($handle, $ffi, $ARecibo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_ConsultarRecibo($handle->cdata, $ARecibo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar recibo", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ConsultaMDFeNaoEnc($handle, $ffi, $nCNPJ, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_ConsultaMDFeNaoEnc($handle->cdata, $nCNPJ, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao consultar não encerrados", 1) != 0)
            return -10;
    }

    $retornoGeral = mb_convert_encoding(FFI::string($sMensagem), "UTF-8", "ISO-8859-1");

    return 0;
}

function Cancelar($handle, $ffi, $AeChave, $AeJustificativa, $AeCNPJCPF, $ALote, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_Cancelar($handle->cdata, $AeChave, $AeJustificativa, $AeCNPJCPF, $ALote, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao cancelar MDFe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EncerrarMDFe($handle, $ffi, $AeChaveOuMDFe, $AeDtEnc, $AcMunicipioDescarga, $AnCNPJ, $AnProtocolo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_EncerrarMDFe($handle->cdata, $AeChaveOuMDFe, $AeDtEnc, $AcMunicipioDescarga, $AnCNPJ, $AnProtocolo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao Encerrar MDFe", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function CarregarEventoINI($handle, $ffi, $eArquivoOuINI, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_CarregarEventoINI($handle->cdata, $eArquivoOuINI);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro carregar o ini do evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEvento($handle, $ffi, $AidLote, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_EnviarEvento($handle->cdata, $AidLote, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar o evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ImprimirEventoPDF($handle, $ffi, $AeArquivoXmlMDFe, $AeArquivoXmlEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_ImprimirEventoPDF($handle->cdata, $AeArquivoXmlMDFe, $AeArquivoXmlEvento);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao imprimir o evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function SalvarEventoPDF($handle, $ffi, $AeArquivoXmlMDFe, $AeArquivoXmlEvento, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
    $retorno = $ffi->MDFE_SalvarEventoPDF($handle->cdata, $AeArquivoXmlMDFe, $AeArquivoXmlEvento, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao salvar o pdf do evento", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function EnviarEmailEvento($handle, $ffi, $AePara, $eArquivoXmlEvento, $eArquivoXmlMDFe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->MDFE_EnviarEmailEvento($handle->cdata, $AePara, $eArquivoXmlEvento, $eArquivoXmlMDFe, $AEnviaPDF, $AeAssunto, $AeCC, $AeAnexos, $AeMensagem);

    if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao enviar e-mail do evento", 1) != 0)
        return -10;

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function DistribuicaoDFePorChave($handle, $ffi, $AeCNPJCPF, $AechMDFe, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
    $retorno = $ffi->MDFE_DistribuicaoDFePorChave($handle->cdata, $AeCNPJCPF, $AechMDFe, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao executar Distribuição por chave", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function DistribuicaoDFePorNSU($handle, $ffi, $AeCNPJCPF, $AeNSU, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
    $retorno = $ffi->MDFE_DistribuicaoDFePorNSU($handle->cdata, $AeCNPJCPF, $AeNSU, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao executar Distribuição por NSU", 1) != 0)
            return -10;
    }

    $retornoGeral = mb_convert_encoding(FFI::string($sMensagem), "UTF-8", "ISO-8859-1");

    return 0;
}

function DistribuicaoDFePorUltNSU($handle, $ffi, $AeCNPJCPF, $AeultNSU, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 144768;
    $sMensagem = FFI::new("char[144768]");
    $retorno = $ffi->MDFE_DistribuicaoDFePorUltNSU($handle->cdata, $AeCNPJCPF, $AeultNSU, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($handle, $ffi, $retorno, $sMensagem, "Erro ao executar Distribuição por UltNSU", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}
