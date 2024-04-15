{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{$I ACBr.inc}

unit ACBrTEFDElgin;

interface

uses
  Classes, 
	SysUtils, 
	ACBrTEFDClass;

const
  CACBrTEFDElgin_ArqTemp   = 'C:\Cliente\req\intpos.tmp' ;
  CACBrTEFDElgin_ArqReq    = 'C:\Cliente\req\intpos.001' ;
  CACBrTEFDElgin_ArqResp   = 'C:\Cliente\resp\intpos.001' ;
  CACBrTEFDElgin_ArqSTS    = 'C:\Cliente\resp\intpos.sts' ;
  CACBrTEFDElgin_GPExeName = 'C:\ELGIN\TEFPassivo\E1_TEFPay_Passivo.exe' ;


type
   { TACBrTEFDElgin }

   TACBrTEFDElgin = class( TACBrTEFDClassTXT )
   public
     constructor Create(AOwner: TComponent); override;
     Function PIX( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean;
   end;

implementation

{ TACBrTEFDClass }

constructor TACBrTEFDElgin.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ArqReq    := CACBrTEFDElgin_ArqReq ;
  ArqResp   := CACBrTEFDElgin_ArqResp ;
  ArqSTS    := CACBrTEFDElgin_ArqSTS ;
  ArqTemp   := CACBrTEFDElgin_ArqTemp ;
  GPExeName := CACBrTEFDElgin_GPExeName ;
  fpTipo    := gpTefElgin;
  Name      := 'TEF_Elgin' ;
end;

function TACBrTEFDElgin.PIX(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; Moeda: Integer): Boolean;
begin
  VerificarTransacaoPagamento( Valor );

  IniciarRequisicao('PIX');
  Req.DocumentoVinculado  := DocumentoVinculado;
  Req.ValorTotal          := Valor;
  Req.Moeda               := Moeda;
  AdicionarIdentificacao;
  FinalizarRequisicao;

  Result := ProcessarRespostaPagamento( IndiceFPG_ECF, Valor);
end;

end.

