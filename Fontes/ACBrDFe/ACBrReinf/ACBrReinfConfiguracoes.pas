{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

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

{$I ACBr.inc}

unit ACBrReinfConfiguracoes;

interface


uses
  Classes, SysUtils, ACBrDFeConfiguracoes, pcnConversaoReinf;

type

  TConfiguracoesReinf = class(TConfiguracoes)
  private
    FVersaoReinf: TpcnVersaoReinf;
    function GetArquivos: TArquivosConf;
    function GetGeral: TGeralConf;
    procedure SetVersaoReinf(const Value: TpcnVersaoReinf);
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesReinf: TConfiguracoesReinf); reintroduce;
  published
    property VersaoReinf: TpcnVersaoReinf read FVersaoReinf write SetVersaoReinf default v1_02_00;
    property Geral: TGeralConf read GetGeral;
    property Arquivos: TArquivosConf read GetArquivos;
    property WebServices;
    property Certificados;
  end;

implementation

{ TConfiguracoesReinf }

procedure TConfiguracoesReinf.Assign(DeConfiguracoesReinf: TConfiguracoesReinf);
begin
  VersaoReinf := DeConfiguracoesReinf.VersaoReinf;
  Geral.Assign(DeConfiguracoesReinf.Geral);
  WebServices.Assign(DeConfiguracoesReinf.WebServices);
  Certificados.Assign(DeConfiguracoesReinf.Certificados);
  Arquivos.Assign(DeConfiguracoesReinf.Arquivos);
end;

constructor TConfiguracoesReinf.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WebServices.ResourceName := 'ACBrReinfServices';
end;

procedure TConfiguracoesReinf.CreateArquivosConf;
begin
  FPArquivos := TArquivosConf.Create(self);
end;

procedure TConfiguracoesReinf.CreateGeralConf;
begin
  FPGeral := TGeralConf.Create(Self);
  FVersaoReinf := v1_02_00;
end;

function TConfiguracoesReinf.GetArquivos: TArquivosConf;
begin
  Result := TArquivosConf(FPArquivos);
end;

function TConfiguracoesReinf.GetGeral: TGeralConf;
begin
  Result := TGeralConf(FPGeral);
end;

procedure TConfiguracoesReinf.SetVersaoReinf(const Value: TpcnVersaoReinf);
begin
  FVersaoReinf := Value;
end;

end.
