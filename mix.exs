defmodule App.MixFile do
  use Mix.Project

  def project do
    [
      app: :app,
      version: "0.0.1",
      deps: deps
    ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: {App, []},
      description: 'minimum app'
    ]
  end

  defp deps do
    [
      # { :some_project, "0.3.0", github: "some_project/other" },
      # { :another_project, "1.0.2", git: "https://example.com/another/repo.git" }
    ]
  end

end
