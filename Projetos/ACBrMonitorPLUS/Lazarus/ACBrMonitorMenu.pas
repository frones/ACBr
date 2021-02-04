{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: 2021 José M. S. Junior                           }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}
{$I ACBr.inc}

unit ACBrMonitorMenu;

interface

uses
  Classes, SysUtils, Contnrs, ComCtrls,
  ACBrMonitorConsts;

const
  C_INDEX_IMG_FOLDER = 30;
  C_INDEX_IMG_ERRO = 31;

type

  { TTela }

  TTela = Class
  private
    FIndice: Integer;
    FNivel: Integer;
    FNome: String;
    FImagemIndice: Integer;
    procedure SetIndice(const Value: Integer);
    procedure SetNivel(const Value: Integer);
    procedure SetNome(const Value: String);
    procedure SetImagemIndice(const Value: Integer);
  public
    property Indice : Integer read FIndice write SetIndice;
    property Nivel : Integer read FNivel write SetNivel;
    property Nome : String read FNome write SetNome;
    property ImagemIndice : Integer read FImagemIndice write SetImagemIndice;

  end;

  { TListaTela }

  TListaTela = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TTela);
    function  GetObject (Index: Integer): TTela;
    procedure Insert (Index: Integer; Obj: TTela);
  public
    function Add (Obj: TTela): Integer;
    property Objects [Index: Integer]: TTela
      read GetObject write SetObject; default;
  end;

  { TMenu }

  TMenu = Class
  private
    FNome: String;
    FControlePagina: TPageControl;
    FArvoreMenu: TTreeView;
    FTela: TTela;
    FTelas: TListaTela;

  public
    Constructor Create(PageControl: TPageControl; TreeView: TTreeView; CarregarMenu: Boolean = True);
    destructor Destroy; override;
    property Nome : String read FNome;
    property Telas : TListaTela read FTelas;
    property ControlePagina: TPageControl read FControlePagina;
    property Tela: TTela read FTela;
    property ArvoreMenu: TTreeView read FArvoreMenu;

    procedure CriarEstruturaMenu;
    procedure CarregarMenuTreeView;
    procedure AbrirTelaTreeView(ANivel, AIndice, AIndicePai: Integer);
    procedure AtualizaItemTela(AIndice, AIndexImg: Integer);
    procedure SincronizaTreeView(AIndice: Integer = -1);
    function AdicionarTelaLista: TTela;

  end;

implementation

{ TMenu }

procedure TMenu.CriarEstruturaMenu;
var
  I,J,K, cont: Integer;
begin
  cont := 0;
  if not Assigned( FControlePagina ) then
    Exit;

  for I := 0 to FControlePagina.PageCount - 1 do
  begin
    FTela := AdicionarTelaLista;
    FTela.Indice:= I;
    FTela.Nivel:= C_LEVEL1;
    FTela.Nome:= FControlePagina.Pages[I].Caption;
    FTela.ImagemIndice:= FControlePagina.Pages[I].ImageIndex;
    FControlePagina.Pages[I].Tag:= cont;
    Inc(cont);

    for J := 0 to  FControlePagina.Pages[I].ControlCount - 1 do
    begin
      if FControlePagina.Pages[I].Controls[J] is TPageControl then
      begin
        for K := 0 to TPageControl(FControlePagina.Pages[I].Controls[J]).PageCount -1 do
        begin
          FTela := AdicionarTelaLista;
          FTela.Indice:= K;
          FTela.Nivel:= C_LEVEL2;
          FTela.Nome:= TPageControl(FControlePagina.Pages[I].Controls[J]).Pages[K].Caption;
          TPageControl(FControlePagina.Pages[I].Controls[J]).Pages[K].Tag:= cont;
          if (TPageControl(FControlePagina.Pages[I].Controls[J]).Pages[K].ImageIndex = -1) then
            FTela.ImagemIndice:= C_INDEX_IMG_FOLDER
          else
            FTela.ImagemIndice:= TPageControl(FControlePagina.Pages[I].Controls[J]).Pages[K].ImageIndex;

          inc(cont);
        end;
      end;
    end;

  end;

end;

procedure TMenu.CarregarMenuTreeView;
var
  NodeComponents, Node, NodeItem: TTreeNode;
  contTela: Integer;
begin

  if not Assigned( FArvoreMenu ) then
    Exit;

  FArvoreMenu.Items.Clear;

  NodeComponents := FArvoreMenu.Items.Add(nil, Nome);
  if Telas.Count > 0 then
  begin
    for contTela := 0 to Telas.Count - 1 do
    begin
      if Telas[contTela].Nivel = C_LEVEL1 then
      begin
        Node := FArvoreMenu.Items.AddChild(NodeComponents, Telas[contTela].Nome );
        Node.ImageIndex := Telas[contTela].ImagemIndice;
        Node.HasChildren:= true;
      end;

      if Telas[contTela].Nivel = C_LEVEL2 then
      begin
        if Assigned(Node) then
        begin
          NodeItem:= FArvoreMenu.Items.AddChild(Node, Telas[contTela].Nome );
          NodeItem.ImageIndex:= Telas[contTela].ImagemIndice;

          if NodeItem.ImageIndex = C_INDEX_IMG_ERRO then
            NodeItem.Parent.Expanded:= True;
        end;

      end;
    end;
  end;
  NodeComponents.Expanded:= true;
  FArvoreMenu.Items.Item[1].Selected:= True;

end;

procedure TMenu.AbrirTelaTreeView(ANivel, AIndice, AIndicePai: Integer);
var
  J: Integer;
begin
  if not Assigned( FControlePagina ) then
    Exit;
  if not Assigned( FArvoreMenu ) then
    Exit;

  case ANivel of
    C_LEVEL1 : FControlePagina.ActivePageIndex := AIndice;
    C_LEVEL2 : begin
                 FControlePagina.ActivePageIndex := AIndicePai;
                 for J := 0 to  FControlePagina.Pages[AIndicePai].ControlCount - 1 do
                 begin
                   if FControlePagina.Pages[AIndicePai].Controls[J] is TPageControl then
                     TPageControl(FControlePagina.Pages[AIndicePai].Controls[J]).ActivePageIndex := AIndice;

                 end;
               end;
    else
      FControlePagina.ActivePageIndex := C_LEVEL0;
  end;

end;

procedure TMenu.AtualizaItemTela(AIndice, AIndexImg: Integer);
begin
  try
    if FTelas.Count > 0 then
      FTelas[AIndice].FImagemIndice:= AIndexImg;
  except
    raise Exception.Create('Indice de Menu invalido!');
  end;

end;

procedure TMenu.SincronizaTreeView(AIndice: Integer);
begin
  if not Assigned( FArvoreMenu ) then
    Exit;

  if not Assigned( FControlePagina ) then
    Exit;

  if AIndice >= 0 then
  begin
    AIndice:= AIndice + 1;
    if FArvoreMenu.Items.Item[AIndice].Level = 1 then
    begin
      FArvoreMenu.FullCollapse;
      FArvoreMenu.Items.Item[0].Expanded:= True;
      FArvoreMenu.Items.Item[AIndice].Expanded:= True;
      if FArvoreMenu.Items.Item[AIndice].SubTreeCount > 1 then
        FArvoreMenu.Items.Item[AIndice +1].Selected:= True
      else
        FArvoreMenu.Items.Item[AIndice].Selected:= True;

      FArvoreMenu.Items.Item[AIndice].Expanded:= True;
    end
    else
      FArvoreMenu.Items.Item[AIndice].Selected:= True;

  end;

end;

constructor TMenu.Create(PageControl: TPageControl; TreeView: TTreeView; CarregarMenu: Boolean);
begin
  FTela := Nil;
  FTelas := TListaTela.Create(true);
  FNome := 'Componentes ACBr';

  if Assigned(PageControl) then
    FControlePagina := PageControl;
  if Assigned(TreeView) then
    FArvoreMenu := TreeView;

  CriarEstruturaMenu;
  if CarregarMenu then
    CarregarMenuTreeView;


end;

destructor TMenu.Destroy;
begin
  FTelas.Free;
  inherited Destroy;
end;

function TMenu.AdicionarTelaLista: TTela;
var
  I: Integer;
begin
  I := FTelas.Add(TTela.Create);
  Result := FTelas[I];
end;

{ TListaTela }

procedure TListaTela.SetObject(Index: Integer; Item: TTela);
begin
  inherited SetItem (Index, Item) ;
end;

function TListaTela.GetObject(Index: Integer): TTela;
begin
  Result := inherited GetItem(Index) as TTela ;
end;

procedure TListaTela.Insert(Index: Integer; Obj: TTela);
begin
  inherited Insert(Index, Obj);
end;

function TListaTela.Add(Obj: TTela): Integer;
begin
  Result := inherited Add(Obj)
end;

{ TTela }

procedure TTela.SetIndice(const Value: Integer);
begin
  FIndice := Value;
end;

procedure TTela.SetNivel(const Value: Integer);
begin
  FNivel := Value;
end;

procedure TTela.SetNome(const Value: String);
begin
  FNome := Value;
end;

procedure TTela.SetImagemIndice(const Value: Integer);
begin
  FImagemIndice := Value;
end;

end.

