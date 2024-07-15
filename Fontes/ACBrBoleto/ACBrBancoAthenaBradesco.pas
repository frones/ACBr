{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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
//incluido 05/07/2023

{$I ACBr.inc}

unit ACBrBancoAthenaBradesco;

interface

uses
  Classes, SysUtils, ACBrBoleto, ACBrBancoBradesco;

type

  { TACBrBancoAthenaBradesco }

  TACBrBancoAthenaBradesco = class(TACBrBancoBradesco)
  public
    procedure GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa:TStringList);  override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
   ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings;


procedure TACBrBancoAthenaBradesco.GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList);
var
  wLinha: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    wLinha:= '0'                                                +  { ID do Registro }
             '1'                                                +  { ID do Arquivo( 1 - Remessa) }
             'REMESSA'                                          +  { Literal de Remessa }
             '01'                                               +  { Código do Tipo de Serviço }
             PadRight('COBRANCA', 15)                           +  { Descrição do tipo de serviço }
             PadLeft(CodigoCedente, 20, '0')                    +  { Codigo da Empresa no Banco }
             PadRight(Nome, 30)                                 +  { Nome da Empresa }
             IntToStrZero(0, 3)                                 +  { Código do Banco 000 }
             PadRight('ATHENABANCO', 15)                        +  { Nome do Banco }
             FormatDateTime('ddmmyy',Now)                       +  { Data de geração do arquivo }
             Space(07)                                          +  { brancos }
             PadLeft(fpCodParametroMovimento, 3 )               +  { Cód. Parâm. Movto }
             IntToStrZero(NumeroRemessa, 7)                     +  { Nr. Sequencial de Remessa  }
             Space(277)                                         +  { brancos }
             IntToStrZero(1, 6);                                   { Nr. Sequencial de Remessa + brancos + Contador }

    ARemessa.Add(UpperCase(wLinha));
  end;

end;

procedure TACBrBancoAthenaBradesco.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  LLinha : String;
begin
   LLinha := GerarLinhaRegistroTransacao400(ACBrTitulo, aRemessa);
   LLinha := Copy(LLinha, 1, 20) +  PadLeft('', 17, '0') + Copy(LLinha, 38,LengthNativeString(LLinha));  // 021 a 037 Identificacao da Empresa Cedente no Banco - Preencher com zeros

   aRemessa.Add(UpperCase(LLinha));
end;

end.
