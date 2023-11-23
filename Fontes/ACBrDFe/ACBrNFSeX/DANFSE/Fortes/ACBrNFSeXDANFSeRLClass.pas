{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrNFSeXDANFSeRLClass;

interface

uses
  SysUtils, Classes,
  ACBrBase,
  ACBrNFSeXClass, ACBrNFSeXDANFSeClass;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFSeXDANFSeRL = class(TACBrNFSeXDANFSeClass)
  protected
    FDetalharServico: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDANFSe(NFSe: TNFSe = nil); override;
    procedure ImprimirDANFSePDF(NFSe: TNFSe = nil); overload; override;
    procedure ImprimirDANFSePDF(AStream: TStream; NFSe: TNFSe = nil); overload; override;

  published
    property DetalharServico: Boolean read FDetalharServico write FDetalharServico default False;
  end;

implementation

uses
  ACBrUtil.FilesIO,
  ACBrNFSeX, ACBrNFSeXConversao, ACBrNFSeXDANFSeRL,
  ACBrNFSeXDANFSeRLRetrato, ACBrNFSeXDANFSeRLSimplISS, ACBrNFSeXDANFSeRLISSNet;

constructor TACBrNFSeXDANFSeRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDetalharServico := False;
end;

destructor TACBrNFSeXDANFSeRL.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrNFSeXDANFSeRL.ImprimirDANFSe(NFSe: TNFSe);
var
  i: Integer;
  Notas: array of TNFSe;
  fqrXDANFSeRLRetrato: TfrlXDANFSeRL;
begin
  SetDadosPrestador;
  Provedor := TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Provedor;

  case Provedor of
    proSimplISS:
      fqrXDANFSeRLRetrato := TfrlXDANFSeRLSimplISS.Create(Self);
    proIssNet:
      fqrXDANFSeRLRetrato := TfrlXDANFSeRLISSNet.Create(Self);
  else
    fqrXDANFSeRLRetrato := TfrlXDANFSeRLRetrato.Create(Self);
  end;

  fqrXDANFSeRLRetrato.QuebradeLinha(TACBrNFSeX(ACBrNFSe).Provider.ConfigGeral.QuebradeLinha);

  if (NFSe = nil) then
  begin
    SetLength(Notas, TACBrNFSeX(ACBrNFSe).NotasFiscais.Count);

    for i := 0 to (TACBrNFSeX(ACBrNFSe).NotasFiscais.Count - 1) do
      Notas[i] := TACBrNFSeX(ACBrNFSe).NotasFiscais.Items[i].NFSe;
  end
  else
  begin
    SetLength(Notas, 1);
    Notas[0] := NFSe;
  end;

  fqrXDANFSeRLRetrato.Imprimir(Self, Notas);

  fqrXDANFSeRLRetrato.Free;
end;

procedure TACBrNFSeXDANFSeRL.ImprimirDANFSePDF(NFSe: TNFSe);
var
  i: integer;
  fqrXDANFSeRLRetrato: TfrlXDANFSeRL;
begin
  SetDadosPrestador;
  Provedor := TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Provedor;

  case Provedor of
    proSimplISS:
      fqrXDANFSeRLRetrato := TfrlXDANFSeRLSimplISS.Create(Self);
    proIssNet:
      fqrXDANFSeRLRetrato := TfrlXDANFSeRLISSNet.Create(Self);
  else
    fqrXDANFSeRLRetrato := TfrlXDANFSeRLRetrato.Create(Self);
  end;

  fqrXDANFSeRLRetrato.QuebradeLinha(TACBrNFSeX(ACBrNFSe).Provider.ConfigGeral.QuebradeLinha);

  if NFSe = nil then
  begin
    for i := 0 to TACBrNFSeX(ACBrNFSe).NotasFiscais.Count - 1 do
    begin
      FPArquivoPDF := DefinirNomeArquivo(Self.PathPDF,
       TACBrNFSeX(ACBrNFSe).NumID[TACBrNFSeX(ACBrNFSe).NotasFiscais.Items[i].NFSe] + '-nfse.pdf',
       self.NomeDocumento);

      fqrXDANFSeRLRetrato.SalvarPDF(Self, TACBrNFSeX(ACBrNFSe).NotasFiscais.Items[i].NFSe, FPArquivoPDF);
    end;
  end
  else
  begin
    FPArquivoPDF := DefinirNomeArquivo(Self.PathPDF,
     TACBrNFSeX(ACBrNFSe).NumID[NFSe] + '-nfse.pdf', self.NomeDocumento);

    fqrXDANFSeRLRetrato.SalvarPDF(Self, NFSe, FPArquivoPDF);
  end;

  fqrXDANFSeRLRetrato.Free;
end;

procedure TACBrNFSeXDANFSeRL.ImprimirDANFSePDF(AStream: TStream; NFSe: TNFSe);
var
  i: integer;
  fqrXDANFSeRLRetrato: TfrlXDANFSeRL;
begin
  SetDadosPrestador;
  Provedor := TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Provedor;

  case Provedor of
    proSimplISS:
      fqrXDANFSeRLRetrato := TfrlXDANFSeRLSimplISS.Create(Self);
    proIssNet:
      fqrXDANFSeRLRetrato := TfrlXDANFSeRLISSNet.Create(Self);
  else
    fqrXDANFSeRLRetrato := TfrlXDANFSeRLRetrato.Create(Self);
  end;

  fqrXDANFSeRLRetrato.QuebradeLinha(TACBrNFSeX(ACBrNFSe).Provider.ConfigGeral.QuebradeLinha);

  if NFSe = nil then
  begin
    for i := 0 to TACBrNFSeX(ACBrNFSe).NotasFiscais.Count - 1 do
      fqrXDANFSeRLRetrato.SalvarPDF(Self, TACBrNFSeX(ACBrNFSe).NotasFiscais.Items[i].NFSe, AStream);
  end
  else
  begin
    fqrXDANFSeRLRetrato.SalvarPDF(Self, NFSe, AStream);
  end;

  fqrXDANFSeRLRetrato.Free;
end;

end.
