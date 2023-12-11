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

{$I ACBr.inc}

unit ACBrNFSeXDANFSeFPDFClass;

interface

uses
  Forms, 
  SysUtils, 
  Classes, 
  ACBrBase, 
  ACBrNFSeXClass, ACBrNFSeXDANFSeClass;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFSeXDANFSeFPDF = class(TACBrNFSeXDANFSeClass)
  private

  protected
//    FPrintDialog: boolean;
//	  FDetalharServico : Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDANFSe(NFSe: TNFSe = nil); override;
    procedure ImprimirDANFSePDF(NFSe: TNFSe = nil); override;
  published
//    property PrintDialog: boolean read FPrintDialog write FPrintDialog;
//    property DetalharServico: Boolean read FDetalharServico write FDetalharServico default False;

  end;

implementation

uses
  ACBrUtil.FilesIO,
  ACBrNFSeX, ACBrNFSeXConversao, ACBr.DANFSeX.Classes, ACBrNFSeXInterface,
  ACBr.DANFSe.FPDF.A4Retrato;

constructor TACBrNFSeXDANFSeFPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

//  FPrintDialog := True;
//  FDetalharServico := False;
  MargemInferior := 0;
  MargemSuperior := 0;
  MargemEsquerda := 0;
  MargemDireita  := 0;
end;

destructor TACBrNFSeXDANFSeFPDF.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrNFSeXDANFSeFPDF.ImprimirDANFSe(NFSe: TNFSe = nil);
//var
//  i: Integer;
//  Notas: array of TNFSe;
begin
  ImprimirDANFSePDF(NFSe);

//  TfrlDANFSeRLRetrato.QuebradeLinha(TACBrNFSe(ACBrNFSe).Configuracoes.Geral.ConfigGeral.QuebradeLinha);
//
//  if (NFSe = nil) then
//  begin
//    SetLength(Notas, TACBrNFSe(ACBrNFSe).NotasFiscais.Count);
//
//    for i := 0 to (TACBrNFSe(ACBrNFSe).NotasFiscais.Count - 1) do
//      Notas[i] := TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe;
//  end
//  else
//  begin
//    SetLength(Notas, 1);
//    Notas[0] := NFSe;
//  end;
//
//  TfrlDANFSeRLRetrato.Imprimir(Self, Notas);
end;

procedure TACBrNFSeXDANFSeFPDF.ImprimirDANFSePDF(NFSe: TNFSe = nil);
Var
//  i: integer;
  aImpressao: TACBrDANFSeFPDFA4Retrato;
  DadosAux: TDadosNecessariosParaDANFSeX;
  FProvider: IACBrNFSeXProvider;
  UmaNFSe: TNFSe;
  nomearquivo: string;
begin
  FProvider := TACBrNFSeX(ACBrNFSe).Provider;
  DadosAux := TDadosNecessariosParaDANFSeX.Create;
  try
    if NFSe = nil then
    begin
      UmaNFSe := TACBrNFSeX(ACBrNFSe).NotasFiscais.Items[0].NFSe;
    end
    else
    begin
      UmaNFSe := NFSe;
    end;

    DadosAux.NaturezaOperacaoDescricao      := FProvider.NaturezaOperacaoDescricao(UmaNFSe.NaturezaOperacao);
    DadosAux.RegimeEspecialDescricao        := FProvider.RegimeEspecialTributacaoDescricao(UmaNFSe.RegimeEspecialTributacao);
    DadosAux.IssAReterDescricao             := FProvider.SituacaoTributariaDescricao(UmaNFSe.Servico.Valores.IssRetido);
    DadosAux.OptanteSimplesDescricao        := FProvider.SimNaoDescricao(UmaNFSe.OptanteSimplesNacional);
    DadosAux.IncentivadorCulturalDescricao  := FProvider.SimNaoDescricao(UmaNFSe.IncentivadorCultural);
    DadosAux.QuebradeLinha := FProvider.ConfigGeral.QuebradeLinha;
    DadosAux.Detalhar      := FProvider.ConfigGeral.DetalharServico;

    aImpressao := TACBrDANFSeFPDFA4Retrato.Create;
    try
      nomearquivo := ChangeFileExt(Application.ExeName, '.pdf');
      aImpressao.SalvarPDF(UmaNFSe, DadosAux, Self, nomearquivo);
    finally
      aImpressao.Free;
    end;
  finally
    DadosAux.Free;
  end;

/////
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


end;

end.
