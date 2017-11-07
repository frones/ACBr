unit Frm_Feriados;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    btnBuscar: TButton;
    Label1: TLabel;
    cboWebService: TComboBox;
    Label2: TLabel;
    edtAno: TEdit;
    edtUF: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtCidade: TEdit;
    mmResultado: TMemo;
    Label5: TLabel;
    edtToken: TEdit;
    Label6: TLabel;
    edtArquivo: TEdit;
    Button1: TButton;
    procedure btnBuscarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure BuscaEfetuadaFeriado(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses
  ACBrFeriado;

{$R *.dfm}

procedure TForm1.BuscaEfetuadaFeriado(Sender: TObject);
var
  Feriados: TACBrFeriado;
  ListaFeriados: TStringList;
  i: Integer;
  Evento: TACBrFeriadoEvento;
  sDiaSemana: String;
  sTipo: String;
  sLegislacao: String;
  sLink: String;
begin
  Feriados := TACBrFeriado(Sender);

  if (Feriados.Eventos.Count > 0) then
  begin
    ListaFeriados := TStringList.Create;
    try
      for i := 0 to Feriados.Eventos.Count - 1 do
      begin
        Evento := Feriados.Eventos.Objects[i];
        if not(Evento.Tipo in [ftNenhum, ftDiaConvencional]) then
        begin
          sDiaSemana := FormatDateTime(', ddd,', Evento.Data);

          case Evento.Tipo of
            ftNacional:    sTipo := '(Feriado Nacional)';
            ftEstadual:    sTipo := '(Feriado Estadual)';
            ftMunicipal:   sTipo := '(Feriado Municipal)';
            ftFacultativo: sTipo := '(Ponto Facultativo)';
          end;
          sLegislacao := '';
          if (Evento.Legislacao <> '') then
            sLegislacao := 'Legislação: '+ Evento.Legislacao;
          sLink := '';
          if (Evento.Link <> '') then
            sLink := '('+ Evento.Link + ')';
          ListaFeriados.Add(DateToStr(Evento.Data) + ' ' +
                            sDiaSemana + ' ' +
                            Evento.Nome + ' ' +
                            sTipo + ' ' +
                            Evento.Descricao + ' ' +
                            sLegislacao + ' ' +
                            sLink);
        end;
      end;
      ListaFeriados.Insert(0, 'Lista de Feriados:');
      mmResultado.Lines.Text := ListaFeriados.Text;
    finally
      ListaFeriados.Free;
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Title := 'Procurar arquivo de feriados e eventos';
    if (edtArquivo.Text <> '') then
      OpenDialog.InitialDir := ExtractFilePath(edtArquivo.Text)
    else
      OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
    OpenDialog.Filter := 'Arquivo JSON|*.json';
    OpenDialog.DefaultExt := 'json';
    if (OpenDialog.Execute) then
      edtArquivo.Text := OpenDialog.FileName;
  finally
    OpenDialog.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Caminho: String;
begin
  Caminho := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'ACBrFeriadoEventos.json';
  if (FileExists(Caminho)) then
    edtArquivo.Text := Caminho;
  edtAno.Text := FormatDateTime('yyyy', Now);
end;

procedure TForm1.btnBuscarClick(Sender: TObject);
var
  Feriados: TACBrFeriado;
begin
  mmResultado.Clear;

  Feriados := TACBrFeriado.Create(nil);
  try
    Feriados.OnBuscaEfetuada := BuscaEfetuadaFeriado;

    case cboWebService.ItemIndex of
      0:
      begin
        Feriados.WebService := wsfCalendario;
        Feriados.Token := edtToken.Text;
      end;
      1:
      begin
        Feriados.WebService := wsfJSON;
        Feriados.PathArquivo := edtArquivo.Text;
      end;
    end;

    Feriados.Buscar(StrToIntDef(edtAno.Text, 0), edtUF.Text, edtCidade.Text);
  finally
    Feriados.Free;
  end;
end;

end.
