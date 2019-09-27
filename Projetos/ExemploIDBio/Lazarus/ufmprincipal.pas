unit UfmPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls, DBGrids, Grids, UdmVenda;

type

  { TfmPrincipal }

  TfmPrincipal = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    edtCodigoItem: TEdit;
    edtQuantidade: TEdit;
    edtValorItem: TEdit;
    edtSubtotal: TEdit;
    edtValorUnit: TEdit;
    Image1: TImage;
    Image2: TImage;
    lblCodItem: TLabel;
    lblQuantidade: TLabel;
    lblValorItem: TLabel;
    lblSubtotal: TLabel;
    lblValorUnitario: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    N1: TMenuItem;
    pnlControliD: TPanel;
    pnlItens: TPanel;
    pnlBot: TPanel;
    pnlEdits: TPanel;
    pnlDescricaoItem: TPanel;
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure edtCodigoItemKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtCodigoItemKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
  private
    procedure InicializaEdits;

  public

  end;

var
  fmPrincipal: TfmPrincipal;

implementation

uses UfmLiberaAcao, LCLType, SCVBioControlID;

{$R *.lfm}

{ TfmPrincipal }

procedure TfmPrincipal.DBGrid1DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if not (gdSelected in State) then
    begin
      if ((Sender as TDBGrid).DataSource.DataSet.Fields[6].Value = True) then
      begin
        (Sender as TDBGrid).Canvas.Brush.Color := clCream;
        (Sender as TDBGrid).Canvas.Font.Style  := [fsBold];
        (Sender as TDBGrid).Canvas.Font.Color  := clRed;
      end
      else if ((Sender as TDBGrid).DataSource.DataSet.RecNo mod 2 <> 0) then
        (Sender as TDBGrid).Canvas.Brush.Color := clWindow
      else
        (Sender as TDBGrid).Canvas.Brush.Color := clBtnFace;

      (Sender as TDBGrid).Canvas.FillRect(Rect);
      (Sender as TDBGrid).defaultdrawcolumncell(rect,datacol,column,state);
    end;
end;

procedure TfmPrincipal.edtCodigoItemKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_UP:
    begin
      dmVenda.tblVenda.Prior;
    end;
    VK_DOWN:
    begin
      dmVenda.tblVenda.Next;
    end;
  end;
end;

procedure TfmPrincipal.edtCodigoItemKeyPress(Sender: TObject; var Key: char);
var
  codigoProduto: Integer;
begin
  if Key = #13 then
  begin
    if TryStrToInt(Trim(edtCodigoItem.Text), codigoProduto) then
    begin
      dmVenda.VendeItem(codigoProduto);
      edtSubtotal.Text := 'R$ '+ FloatToStr(dmVenda.Subtotal);
      InicializaEdits;
    end
    else
      ShowMessage('Código inválido');
  end;

end;

procedure TfmPrincipal.FormCreate(Sender: TObject);
begin
  InicializaEdits;
end;

procedure TfmPrincipal.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TfmPrincipal.FormKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TfmPrincipal.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F11 then
  begin
    if fmLiberaAcao.PermitirAcao then
    begin
      edtSubtotal.Text := 'R$ '+ FloatToStr(dmVenda.Subtotal);
      dmVenda.CancelaItem;
    end;
  end;
end;

procedure TfmPrincipal.MenuItem2Click(Sender: TObject);
begin
  if MessageDlg('Vamos sentir sua falta...', 'Deseja Sair?', mtConfirmation, mbYesNo,0, mbNo) = mrYes then
  begin
    Close;
  end;
end;

procedure TfmPrincipal.MenuItem3Click(Sender: TObject);
var
  stemplate: string;
  FileTemplate: TStringList;
begin
  if MessageDlg('Por favor, confime:', 'Deseja sobrescrever a digital padrão?', mtConfirmation, mbYesNo,0, mbNo) <> mrYes then
  begin
    Exit;
  end;

  stemplate := RetornarDigital();
  if stemplate <> '' then
  begin
    FileTemplate := TStringList.Create;
    try
      FileTemplate.add(stemplate);
      FileTemplate.SaveToFile('Digital.txt');
      ShowMessage('Digital salva com sucesso');
    finally
      FileTemplate.Free;
    end;
  end
  else
    ShowMessage('Não foi possível capturar a digital...');
end;

procedure TfmPrincipal.InicializaEdits;
begin
  edtCodigoItem.Text := '';
  edtQuantidade.Text := '1,000';
  edtValorUnit.Text  := '';
  edtValorItem.Text  := '';
end;

end.

