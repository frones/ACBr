{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrDFeComum.SignatureClass;

interface

uses
  SysUtils, Classes;

type

  { TSignature }

  TSignature = class(TPersistent)
  private
    FURI: string;
    FDigestValue: string;
    FSignatureValue: string;
    FX509Certificate: string;
  public
    procedure Assign(Source: TPersistent); override;

    procedure Clear;
  published
    property URI: string             read FURI             write FURI;
    property DigestValue: string     read FDigestValue     write FDigestValue;
    property SignatureValue: string  read FSignatureValue  write FSignatureValue;
    property X509Certificate: string read FX509Certificate write FX509Certificate;
  end;

implementation

{ TSignature }

procedure TSignature.Clear;
begin
  FURI := '';
  FDigestValue := '';
  FSignatureValue := '';
  FX509Certificate := '';
end;

procedure TSignature.Assign(Source: TPersistent);
begin
  if Source is TSignature then
  begin
    URI := TSignature(Source).URI;
    DigestValue := TSignature(Source).DigestValue;
    SignatureValue := TSignature(Source).SignatureValue;
    X509Certificate := TSignature(Source).X509Certificate;
  end
  else
    inherited; 
end;

end.

