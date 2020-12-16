# -*- coding: UTF-8 -*-
require "msf/core"

class MetasploitModule < Msf::Auxiliary
  def initialize(info = {})
    super(update_info(info,
                      "Name" => "Module name",
                      "Description" => %q{
        Say something that the user might want to know.
      },
                      "Author" => ["Name"],
                      "License" => MSF_LICENSE))
  end

  def run
    # Main function
  end
end
