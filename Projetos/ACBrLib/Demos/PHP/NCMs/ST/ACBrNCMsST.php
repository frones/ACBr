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
    $retorno = $ffi->NCM_Inicializar($iniPath, "");

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao inicializar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function Finalizar($ffi)
{
    $retorno = $ffi->NCM_Finalizar();

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao finalizar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function ConfigLer($ffi, $eArqConfig)
{
    $retorno = $ffi->NCM_ConfigLer($eArqConfig);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao carregar o arquivo ini. ") != 0)
        return -10;

    return 0;
}

function ConfigLerValor($ffi, $eSessao, $eChave, &$sValor)
{
    $sResposta = FFI::new("char[9048]");
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $retorno = $ffi->NCM_ConfigLerValor($eSessao, $eChave, $sResposta, FFI::addr($esTamanho));

    $sMensagem = FFI::new("char[9048]");

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao ler valor na secao[$eSessao] e chave[$eChave]. ", 1) != 0)
            return -10;
    }

    $sValor = FFI::string($sResposta);
    return 0;
}

function ConfigGravarValor($ffi, $eSessao, $eChave, $value)
{
    $retorno = $ffi->NCM_ConfigGravarValor($eSessao, $eChave, $value);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao gravar valores [$value] na secao[$eSessao] e chave[$eChave]. ") != 0)
        return -10;

    return 0;
}

function ConfigGravar($ffi, $eArqConfig)
{
    $retorno = $ffi->NCM_ConfigGravar($eArqConfig);
    $sMensagem = FFI::new("char[9048]");

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao gravar as configurações. ") != 0)
        return -10;

    return 0;
}

function UltimoRetorno($ffi, $retornolib, &$sMensagem, $msgErro, $retMensagem = 0)
{
    if (($retornolib !== 0) || ($retMensagem == 1)) {
        $esTamanho = FFI::new("long");
        $esTamanho->cdata = 9048;
        $resposta = $ffi->NCM_UltimoRetorno($sMensagem, FFI::addr($esTamanho));

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

function NCMNome($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->NCM_Nome($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao verificar o nome da biblioteca", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function NCMVersao($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->NCM_Versao($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao verificar a versão da biblioteca", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function OpenSSLInfo($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->NCM_OpenSSLInfo($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao verificar a OpenSSLInfo", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function ObterNCMs($ffi, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->NCM_ObterNCMs($sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao obter NCMs", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function BaixarLista($ffi, $cNomeArquivo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->NCM_BaixarLista($cNomeArquivo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao baixar lista de NCMs", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);

    return 0;
}

function BuscarPorCodigo($ffi, $cNCM, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->NCM_BuscarPorCodigo($cNCM, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao filtrar por Código", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);
    $retornoGeral = str_replace("|", PHP_EOL, $retornoGeral);

    return 0;
}

function BuscarPorDescricao($ffi, $cDesc, $nTipo, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->NCM_BuscarPorDescricao($cDesc, $nTipo, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao filtrar por Descrição", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);
    $retornoGeral = str_replace("|", PHP_EOL, $retornoGeral);

    return 0;
}

function Validar($ffi, $cNCM, &$retornoGeral)
{
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $sMensagem = FFI::new("char[9048]");
    $retorno = $ffi->NCM_Validar($cNCM, $sMensagem, FFI::addr($esTamanho));

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao validar NCM", 1) != 0)
            return -10;
    }

    $retornoGeral = FFI::string($sMensagem);
    //$retornoGeral = str_replace("|", PHP_EOL, $retornoGeral);

    return 0;
}



