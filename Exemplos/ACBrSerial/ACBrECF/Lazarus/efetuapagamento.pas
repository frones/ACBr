{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
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

unit EfetuaPagamento;

{$mode objfpc}{$H+}

interface

uses
  ACBrECF,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, strutils;

type

  { TfrPagamento }

  TfrPagamento = class(TForm)
    mFormas: TMemo;
    Label1: TLabel;
    edCod: TEdit;
    Label2: TLabel;
    edValor: TEdit;
    btImprimir: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lTotalAPAGAR: TLabel;
    lTotalPago: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    cbVinc: TCheckBox;
    Label8: TLabel;
    edObs: TEdit;
    SpeedButton1: TSpeedButton;
    procedure edValorKeyPress(Sender: TObject; var Key: Char);
    procedure btImprimirClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    procedure AtualizaVal ;
    Procedure CarregaFPG ;
  public
    { Public declarations }
    TipoCupom : Char ;
  end;

var
  frPagamento: TfrPagamento;

implementation

uses ECFTeste1, ACBrECFClass, ACBrUtil;

procedure TfrPagamento.edValorKeyPress(Sender: TObject; var Key: Char);
begin
  if Key in [',','.'] then
     Key := DecimalSeparator ;
end;

procedure TfrPagamento.btImprimirClick(Sender: TObject);
begin
  if TipoCupom <> 'N' then
   begin
     Form1.ACBrECF1.EfetuaPagamento( edCod.Text, StrToFloat( edValor.Text),
                                     edObs.Text ,cbVinc.Checked );
     Form1.mResp.Lines.Add( 'Efetua Pagamento: '+edCod.Text +
                            ' Valor: '+edValor.Text +
                            ' Obs: '+edObs.Text +
                            ' Vinc: '+IfThen(cbVinc.Checked,'S','N') );
   end
  else
   begin
     Form1.ACBrECF1.EfetuaPagamentoNaoFiscal( edCod.Text, StrToFloat( edValor.Text),
                                     edObs.Text ,cbVinc.Checked );
     Form1.mResp.Lines.Add( 'Efetua Pagamento Não Fiscal: '+edCod.Text +
                            ' Valor: '+edValor.Text +
                            ' Obs: '+edObs.Text +
                            ' Vinc: '+IfThen(cbVinc.Checked,'S','N') );
   end ;

  Form1.AtualizaMemos ;
  AtualizaVal ;
end;

procedure TfrPagamento.FormShow(Sender: TObject);
begin
  AtualizaVal ;
  CarregaFPG ;
  TipoCupom := 'F' ;
end;

procedure TfrPagamento.AtualizaVal;
begin
  lTotalAPAGAR.Caption := FloatToStr( Form1.ACBrECF1.Subtotal ) ;
  lTotalPago.Caption   := FloatToStr( Form1.ACBrECF1.TotalPago ) ;
end;

procedure TfrPagamento.SpeedButton1Click(Sender: TObject);
Var Descricao : String ;
    FPG : TACBrECFFormaPagamento ;  { Necessita de uses ACBrECF }
begin
  Descricao := '';
  if InputQuery('Pesquisa Descrição Forma Pagamento',
                'Entre com a Descrição a Localizar ou Cadastrar(Bematech)',
                Descricao) then
  begin
     FPG := Form1.ACBrECF1.AchaFPGDescricao( Descricao ) ;

     if FPG = nil then
        raise Exception.Create('Forma de Pagamento: '+Descricao+
                               ' não encontrada') ;

     edCod.Text := FPG.Indice ;

     { Bematech permite cadastrar formas de Pagamento dinamicamente }
     if (Form1.ACBrECF1.ModeloStr = 'Bematech') and
        (pos( FPG.Descricao, mFormas.Text ) = 0) then
        CarregaFPG ;
  end ;

end;

procedure TfrPagamento.CarregaFPG;
Var A : Integer ;
begin
  mFormas.Clear ;
  with Form1 do
  begin
     { Bematech e NaoFiscal permitem cadastrar formas de Pagamento dinamicamente }
     if (Form1.ACBrECF1.Modelo in [ecfBematech,ecfNaoFiscal])then
        ACBrECF1.CarregaFormasPagamento
     else
        ACBrECF1.AchaFPGIndice('') ;  { força carregar, se ainda nao o fez }

     for A := 0 to ACBrECF1.FormasPagamento.Count -1 do
     begin
        mFormas.Lines.Add( ACBrECF1.FormasPagamento[A].Indice+' -> '+
              ACBrECF1.FormasPagamento[A].Descricao+' - '+IfThen(
              ACBrECF1.FormasPagamento[A].PermiteVinculado,'v',''));
     end ;
  end ;

end;

initialization
  {$I efetuapagamento.lrs}

end.

