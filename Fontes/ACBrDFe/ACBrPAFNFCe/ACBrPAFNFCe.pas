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

unit ACBrPAFNFCe;

interface

uses
  Classes, SysUtils, ACBrBase,
  ACBrDFe, ACBrDFeConfiguracoes,
  ACBrPAFNFCe_MenuFiscal, ACBrPAFNFCe_Comum,
  ACBrUtil.FilesIO;

type

  { TConfiguracoesPAFNFCe }

  TConfiguracoesPAFNFCe = class(TConfiguracoes)
  public
  published
    property Geral;
    property Certificados;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPAFNFCe = class(TACBrDFe)
  private
    FMenuFiscal: TACBrPAFNFCe_MenuFiscal;
    function GetConfiguracoes: TConfiguracoesPAFNFCe;
    procedure SetConfiguracoes(const Value: TConfiguracoesPAFNFCe);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MenuFiscal: TACBrPAFNFCe_MenuFiscal read FMenuFiscal write FMenuFiscal;

  published
    property Configuracoes: TConfiguracoesPAFNFCe read GetConfiguracoes Write SetConfiguracoes;
  end;

implementation

{ TACBrPAFNFCe }

constructor TACBrPAFNFCe.Create(AOwner: TComponent);
begin
  inherited;
  FMenuFiscal := TACBrPAFNFCe_MenuFiscal.Create(Self);
end;

destructor TACBrPAFNFCe.Destroy;
begin
  FMenuFiscal.Free;
  inherited;
end;

function TACBrPAFNFCe.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesPAFNFCe.Create(Self);
end;

function TACBrPAFNFCe.GetConfiguracoes: TConfiguracoesPAFNFCe;
begin
  Result := TConfiguracoesPAFNFCe(FPConfiguracoes);
end;

procedure TACBrPAFNFCe.SetConfiguracoes(const Value: TConfiguracoesPAFNFCe);
begin
  FPConfiguracoes := Value;
end;

end.
