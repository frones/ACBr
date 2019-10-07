{******************************************************************************}
{ Projeto: Componente ACBrLCDPR                                                }
{  Biblioteca multiplataforma de componentes Delphi para geração do LCDPR -    }
{ Lirvro Caixa Digital do Produtor Rural                                       }
{                                                                              }
{                                                                              }
{ Desenvolvimento e doação ao Projeto ACBr: Willian Hübner                     }
{                                                                              }
{ Ajustes e correções para doação: Elton Barbosa (EMBarbosa)                   }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}
unit LCDPRBlocos;

interface

type
  TCodVer = (Versao001);
  TIndInicio = (indRegular, indAbertura, indInicioObriga);
  TIndSitEsp = (iseNormal, iseFalecimento, iseEspolio, iseSaidaDefinitiva);
  TFormaApur = (faLivroCaixa, faApurLucro);
  TTipoExploracao = (teExploracaoInd, teCondominio, teImovelArrendado, teParceria, teComodato);
  TTipoContraparte = (tpcCondomino, tpcArrendante, tpcParceiro, tpcComodatario);
  TTipoDoc = (tdNotaFiscal, tdFatura, tdRecibo, tdContrato, tdFolhaPagamento, tdOutros);
  TTipoLanc = (tlReceitaRural, tlDespesaCusteio, tlDespesaNaoDedutivel, tlProdEntregue, tlAdiantamentos);

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
    faLivroCaixa  : Result := '0';
    faApurLucro   : Result := '1';
  end;
end;

function TipoExploracaoToStr(TipoExploracao : TTipoExploracao) : String;
begin
  case TipoExploracao of
    teExploracaoInd   : Result := '0';
    teCondominio      : Result := '1';
    teImovelArrendado : Result := '2';
    teParceria        : Result := '3';
    teComodato        : Result := '4';
  end;
end;

function TipoContraparteToStr(TipoContraparte : TTipoContraparte) : String;
begin
  case TipoContraparte of
    tpcCondomino    : Result := '0';
    tpcArrendante   : Result := '1';
    tpcParceiro     : Result := '2';
    tpcComodatario  : Result := '3';
  end;
end;

function TipoDocToStr(TipoDoc : TTipoDoc) : String;
begin
  case TipoDoc of
    tdNotaFiscal    : Result := '0';
    tdFatura        : Result := '1';
    tdRecibo        : Result := '2';
    tdContrato      : Result := '3';
    tdFolhaPagamento: Result := '4';
    tdOutros        : Result := '5';
  end;
end;

function TipoLancToStr(TipoLanc : TTipoLanc) : String;
begin
  case TipoLanc of
    tlReceitaRural        : Result := '0';
    tlDespesaCusteio      : Result := '1';
    tlDespesaNaoDedutivel : Result := '2';
    tlProdEntregue        : Result := '3';
    tlAdiantamentos       : Result := '4';
  end;
end;

end.
