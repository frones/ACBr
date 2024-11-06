{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
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

//incluido
unit ACBrBancoCora;

interface

uses
  ACBrBoleto,
  Classes,
  SysUtils;

type

  { TACBrBancoCora }
  EACBrBoletoCoraException = class(Exception);
  TACBrBancoCora = class(TACBrBancoClass)
  private
    function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
  public
    constructor create(AOwner: TACBrBanco);
    function GerarRegistroHeader240(NumeroRemessa : Integer): String;    override;
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa:TStringList);  override;
    Procedure LerRetorno400(ARetorno:TStringList); override;
    Procedure LerRetorno240(ARetorno:TStringList); override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
  end;

implementation

uses
  ACBrUtil.Strings,
  StrUtils, ACBrUtil.Base;

const METODO_NAO_IMPLEMENTADO = 'ESSE METODO %s %s NÃO ESTÁ IMPLEMENTADO NESSA CLASSE';
{ TACBrBancoCora }

constructor TACBrBancoCora.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                 := 9;
   fpNome                   := 'CORA';
   fpNumero                 := 403;
   fpTamanhoMaximoNossoNum  := 10;
   fpTamanhoCarteira        := 2;
end;

function TACBrBancoCora.MontarCampoCodigoCedente(
  const ACBrTitulo: TACBrTitulo): String;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia;
end;

function TACBrBancoCora.MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String;
begin
  Result := Copy(onlyNumber(ACBrTitulo.ACBrBoleto.Cedente.CNPJCPF), 1, 8)
            + ACBrTitulo.NossoNumero;
end;

function TACBrBancoCora.MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String;
begin
  Result := IntToStrZero(ACBrTitulo.ACBrBoleto.Banco.Numero, 3)
           + '9'
           + CalcularFatorVencimento(ACBrTitulo.Vencimento)
           + IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10)
           + Poem_Zeros('0',5)
           + MontarCampoNossoNumero(ACBrTitulo)
           + ACBrTitulo.Carteira;
  Result:= Copy( Result, 1, 4) + CalcularDigitoCodigoBarras(Result) + Copy( Result, 5, 44) ;
end;

function TACBrBancoCora.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  raise EACBrBoletoCoraException.Create(Format(METODO_NAO_IMPLEMENTADO, [ClassName, 'Remessa 240']));
end;

procedure TACBrBancoCora.GerarRegistroHeader400(NumeroRemessa: Integer;
  ARemessa: TStringList);
begin
  raise EACBrBoletoCoraException.Create(Format(METODO_NAO_IMPLEMENTADO, [ClassName, 'Remessa 400']));
end;

procedure TACBrBancoCora.LerRetorno240(ARetorno: TStringList);
begin
  raise EACBrBoletoCoraException.Create(Format(METODO_NAO_IMPLEMENTADO, [ClassName, 'Retorno 240']));
end;

procedure TACBrBancoCora.LerRetorno400(ARetorno: TStringList);
begin
  raise EACBrBoletoCoraException.Create(Format(METODO_NAO_IMPLEMENTADO, [ClassName, 'Retorno 400']));
end;

end.


