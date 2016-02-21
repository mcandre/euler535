guard :shell do
  watch(/^*\.py$/) do |m|
    title = 'test'
    eager "python #{m[0]} | head -n 20"
    status = ($CHILD_STATUS.success? && :success) || :failed
    n '', title, status
    ''
  end
end
