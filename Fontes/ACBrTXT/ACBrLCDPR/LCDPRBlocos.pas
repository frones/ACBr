{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Willian Hübner e Elton Barbosa (EMBarbosa)      }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}
unit LCDPRBlocos;

interface

type
  TCodVer = (Versao001, Versao011);
  TIndInicio = (indRegular, indAbertura, indInicioObriga);
  TIndSitEsp = (iseNormal, iseFalecimento, iseEspolio, iseSaidaDefinitiva);
  TFormaApur = (faLivroCaixa, faApurLucro);
  TTipoExploracao = (teExploracaoInd, teCondominio, teImovelArrendado, teParceria, teComodato, teOutro);
  TTipoContraparte = (tpcCondomino, tpcArrendante, tpcParceiro, tpcComodatario, tpcOutro);
  TTipoDoc = (tdNotaFiscal, tdFatura, tdRecibo, tdContrato, tdFolhaPagamento, tdOutros);
  TTipoLanc = (tlReceitaRural, tlDespesaCusteio, tlProdEntregue);

function CodVerToStr(CodVer : TCodVer) : String;
function IndInicioToStr(IndInicio : TIndInicio) : String;
function IndSitEspToStr(IndSitEsp : TIndSitEsp) : String;
function IndFormaApurToStr(IndFormaApur : TFormaApur) : String;
function TipoExploracaoToStr(TipoExploracao : TTipoExploracao) : String;
function TipoContraparteToStr(TipoContraparte : TTipoContraparte) : String;
function TipoDocToStr(TipoDoc : TTipoDoc) : String;
function TipoLancToStr(TipoLanc : TTipoLanc) : String;

implementation

function CodVerToStr(CodVer : TCodVer) : String;
begin
  case CodVer of
    Versao001 : Result := '0001';
    Versao011 : Result := '0011';
  end;
end;

function IndInicioToStr(IndInicio : TIndInicio) : String;
begin
  case IndInicio of
    indRegular      : Result := '0';
    indAbertura     : Result := '1';
    indInicioObriga : Result := '2';
  end;
end;

function IndSitEspToStr(IndSitEsp : TIndSitEsp) : String;
begin
  case IndSitEsp of
    iseNormal           : Result := '0';
    iseFalecimento      : Result := '1';
    iseEspolio          : Result := '2';
    iseSaidaDefinitiva  : Result := '3';
  end;
end;

function IndFormaApurToStr(IndFormaApur : TFormaApur) : String;
begin
  case IndFormaApur of
    faLivroCaixa  : Result := '1';
    faApurLucro   : Result := '2';
  end;
end;

function TipoExploracaoToStr(TipoExploracao : TTipoExploracao) : String;
begin
  case TipoExploracao of
    teExploracaoInd   : Result := '1';
    teCondominio      : Result := '2';
    teImovelArrendado : Result := '3';
    teParceria        : Result := '4';
    teComodato        : Result := '5';
    teOutro           : Result := '6';
  end;
end;

function TipoContraparteToStr(TipoContraparte : TTipoContraparte) : String;
begin
  case TipoContraparte of
    tpcCondomino    : Result := '1';
    tpcArrendante   : Result := '2';
    tpcParceiro     : Result := '3';
    tpcComodatario  : Result := '4';
    tpcOutro        : Result := '5';
  end;
end;

function TipoDocToStr(TipoDoc : TTipoDoc) : String;
begin
  case TipoDoc of
    tdNotaFiscal    : Result := '1';
    tdFatura        : Result := '2';
    tdRecibo        : Result := '3';
    tdContrato      : Result := '4';
    tdFolhaPagamento: Result := '5';
    tdOutros        : Result := '6';
  end;
end;

function TipoLancToStr(TipoLanc : TTipoLanc) : String;
begin
  case TipoLanc of
    tlReceitaRural        : Result := '1';
    tlDespesaCusteio      : Result := '2';
    tlProdEntregue        : Result := '3';
  end;
end;

end.
