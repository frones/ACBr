{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Ederson Selvati                                 }
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

unit ACBrBancoPenseBank;

interface

uses
  Classes, SysUtils, Contnrs,
  ACBrBoleto, ACBrBoletoConversao, ACBrBancoBrasil;

const
  CACBrBancoPense_Versao = '0.0.1';

type
  { TACBrBancoPenseBank}

  TACBrBancoPenseBank = class(TACBrBancoBrasil)
  private
    function FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String; reintroduce;
  public
   Constructor create(AOwner: TACBrBanco);
   function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
   function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;

  end;

implementation

uses {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
  StrUtils, Variants, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrUtil.DateTime, Math;

constructor TACBrBancoPenseBank.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 9;
   fpNome                  := 'Pense Bank';
   fpNumero                := 001;
   fpTamanhoMaximoNossoNum := 0;
   fpTamanhoConta          := 12;
   fpTamanhoAgencia        := 4;
   fpTamanhoCarteira       := 2;
   fpCodigosMoraAceitos    := '123';

end;

function TACBrBancoPenseBank.FormataNossoNumero(const ACBrTitulo: TACBrTitulo): String;
begin
  Result:= inherited FormataNossoNumero(ACBrTitulo);
  if (ACBrTitulo.ACBrBoleto.Banco.TipoCobranca = cobBancoDoBrasilAPI) then
     Result :=  '000' + Result;

end;

function TACBrBancoPenseBank.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras :String;
  ANossoNumero, AConvenio: String;
  wTamNossNum: Integer;
begin
   AConvenio    := ACBrTitulo.ACBrBoleto.Cedente.Convenio;
   ANossoNumero := FormataNossoNumero(ACBrTitulo);
   wTamNossNum  := CalcularTamMaximoNossoNumero(ACBrTitulo.Carteira,
                                                ACBrTitulo.NossoNumero);

   {Codigo de Barras}
   with ACBrTitulo.ACBrBoleto do
   begin
      FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

      ANossoNumero := Copy(ANossoNumero,4,Length(ANossoNumero));

      if ((ACBrTitulo.Carteira = '18') or (ACBrTitulo.Carteira = '16')) and
         (Length(AConvenio) = 6) and (wTamNossNum = 17) then
       begin
         CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                         '9' +
                         FatorVencimento +
                         IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                         AConvenio + ANossoNumero + '21';
       end
      else
       begin
         CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                         '9' +
                         FatorVencimento +
                         IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                         IfThen((Length(AConvenio) = 7), '000000', '') +
                         ANossoNumero +
                         IfThen((Length(AConvenio) < 7), PadLeft(OnlyNumber(Cedente.Agencia), 4, '0'), '') +
                         IfThen((Length(AConvenio) < 7), IntToStrZero(StrToIntDef(OnlyNumber(Cedente.Conta),0),8), '') +
                         ACBrTitulo.Carteira;
       end;


      DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   end;

   Result:= copy( CodigoBarras, 1, 4) + DigitoCodBarras + copy( CodigoBarras, 5, 44) ;
end;

function TACBrBancoPenseBank.MontarCampoCodigoCedente(
  const ACBrTitulo: TACBrTitulo): String;
begin
    Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'/'+
             IntToStr(StrToIntDef(ACBrTitulo.ACBrBoleto.Cedente.Conta,0));
end;


end.
