{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes 
*******************************************************************************}

unit pcnReinfR2099_Class;

interface

uses
 Classes, Sysutils, pcnConversaoReinf, Controls, Contnrs, pcnReinfClasses;

type
  { TideRespInf }
  TideRespInf = class
  private
    FnmResp: string;
    FcpfResp: string;
    Ftelefone: string;
    Femail: string;
  public
    property nmResp: string read FnmResp write FnmResp;
    property cpfResp: string read FcpfResp write FcpfResp;
    property telefone: string read Ftelefone write Ftelefone;
    property email: string read Femail write Femail;
  end;

  { TinfoFech }
  TinfoFech = class
  private
    FevtServTm: tpSimNao;
    FevtServPr: tpSimNao;
    FevtAssDespRec: tpSimNao;
    FevtAssDespRep: tpSimNao;
    FevtComProd: tpSimNao;
    FevtCPRB: tpSimNao;
    FevtPgtos: tpSimNao;
    FcompSemMovto: string;
  public
    property evtServTm: tpSimNao read FevtServTm write FevtServTm;
    property evtServPr: tpSimNao read FevtServPr write FevtServPr;
    property evtAssDespRec: tpSimNao read FevtAssDespRec write FevtAssDespRec;
    property evtAssDespRep: tpSimNao read FevtAssDespRep write FevtAssDespRep;
    property evtComProd: tpSimNao read FevtComProd write FevtComProd;
    property evtCPRB: tpSimNao read FevtCPRB write FevtCPRB;
    property evtPgtos: tpSimNao read FevtPgtos write FevtPgtos;
    property compSemMovto: string read FcompSemMovto write FcompSemMovto;
  end;

implementation

end.

