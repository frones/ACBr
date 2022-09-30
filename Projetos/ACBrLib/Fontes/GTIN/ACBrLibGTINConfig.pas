{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibGTINConfig;

interface

uses
  Classes, SysUtils, IniFiles, synachar,
  ACBrGTINConfiguracoes,
  ACBrLibConfig;

type
  { TLibGTINConfig }
  TLibGTINConfig = class(TLibConfig)
    private
      FGTINConfig: TConfiguracoesGTIN;

    protected
      procedure Travar; override;
      procedure Destravar; override;

      procedure INIParaClasse; override;
      procedure ClasseParaINI; override;
      procedure ClasseParaComponentes; override;

    public
      constructor Create (AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
      destructor Destroy; Override;

      property GTINConfig: TConfiguracoesGTIN read FGTINConfig;

  end;

implementation

uses
  ACBrLibGTINBase, ACBrLibGTINConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TLibGTINConfig }

constructor TLibGTINConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FGTINConfig := TConfiguracoesGTIN.Create(Nil);
  FGTINConfig.ChaveCryptINI := AChaveCrypt;

end;

destructor TLibGTINConfig.Destroy;
begin
  FGTINConfig.Destroy;

  inherited Destroy;
end;

procedure TLibGTINConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FGTINConfig.ChaveCryptINI := ChaveCrypt;
  FGTINConfig.LerIni(Ini);
end;

procedure TLibGTINConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FGTINConfig.ChaveCryptINI:=ChaveCrypt;
  FGTINConfig.GravarIni(Ini);
end;

procedure TLibGTINConfig.ClasseParaComponentes;
begin
  FGTINConfig.ChaveCryptINI := ChaveCrypt;

  if Assigned(Owner) then
  TACBrLibGTIN(Owner).GTINDM.AplicarConfiguracoes;
end;

procedure TLibGTINConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibGTIN(Owner) do
     GTINDM.Travar;
  end;
end;

procedure TLibGTINConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibGTIN(Owner) do
     GTINDM.Destravar;
  end;
end;

end.

